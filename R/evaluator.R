#' Feature flag evaluator (FDv2 flag/segment JSON)
#'
#' @keywords internal
NULL

.ld_LONG_SCALE <- 2^60 - 1 # Python __LONG_SCALE__; first 15 hex digits of SHA1

#' Evaluate a flag against a context using store lookups
#'
#' @param flag Flag list from store.
#' @param context `ld_context`.
#' @param default Default value.
#' @param get_flag function(key) -> flag|NULL
#' @param get_segment function(key) -> segment|NULL
#' @return `ld_evaluation_detail`
#' @keywords internal
ld_evaluate_flag <- function(flag, context, default, get_flag, get_segment) {
  state <- new.env(parent = emptyenv())
  state$prereq_stack <- character()
  state$segment_stack <- character()
  state$original_flag_key <- flag$key
  state$events <- list()

  detail <- tryCatch(
    .ld_eval_flag(flag, context, state, get_flag, get_segment),
    error = function(e) {
      msg <- conditionMessage(e)
      kind <- if (grepl("attribute reference|did not specify an attribute|circular ", msg)) {
        "MALFORMED_FLAG"
      } else {
        "EXCEPTION"
      }
      evaluation_detail(default, NULL, list(kind = "ERROR", errorKind = kind))
    }
  )
  if (is.null(detail$value) && is.null(detail$variation_index) &&
      identical(detail$reason$kind, "ERROR") && identical(detail$reason$errorKind, "MALFORMED_FLAG")) {
    detail$value <- default
  }
  # If evaluation produced NULL value with no variation, use default for variation()
  if (is.null(detail$value) && is.null(detail$variation_index) &&
      !identical(detail$reason$kind, "ERROR")) {
    detail$value <- default
  }
  detail
}

.ld_get_variation <- function(flag, variation, reason) {
  vars <- flag$variations %||% list()
  idx <- as.integer(variation)
  if (is.na(idx) || idx < 0L || idx >= length(vars)) {
    return(evaluation_detail(NULL, NULL, list(kind = "ERROR", errorKind = "MALFORMED_FLAG")))
  }
  evaluation_detail(vars[[idx + 1L]], idx, reason)
}

.ld_get_off_value <- function(flag, reason) {
  ov <- flag$offVariation
  if (is.null(ov)) return(evaluation_detail(NULL, NULL, reason))
  .ld_get_variation(flag, ov, reason)
}

.ld_eval_flag <- function(flag, context, state, get_flag, get_segment) {
  if (!isTRUE(flag$on)) {
    return(.ld_get_off_value(flag, list(kind = "OFF")))
  }

  prereq_fail <- .ld_check_prerequisites(flag, context, state, get_flag, get_segment)
  if (!is.null(prereq_fail)) {
    return(.ld_get_off_value(flag, prereq_fail))
  }

  target <- .ld_check_targets(flag, context)
  if (!is.null(target)) return(target)

  rules <- flag$rules %||% list()
  if (length(rules) > 0L) {
    for (i in seq_along(rules)) {
      rule <- rules[[i]]
      if (.ld_rule_matches(rule, context, state, get_segment)) {
        reason <- list(kind = "RULE_MATCH", ruleIndex = i - 1L)
        if (!is.null(rule$id)) reason$ruleId <- rule$id
        return(.ld_value_for_vr(flag, rule, context, reason))
      }
    }
  }

  .ld_value_for_vr(flag, flag$fallthrough %||% list(), context, list(kind = "FALLTHROUGH"))
}

.ld_check_prerequisites <- function(flag, context, state, get_flag, get_segment) {
  prereqs <- flag$prerequisites %||% list()
  if (length(prereqs) == 0L) return(NULL)

  flag_key <- flag$key
  if (!identical(flag_key, state$original_flag_key)) {
    state$prereq_stack <- c(state$prereq_stack, flag_key)
  }

  for (prereq in prereqs) {
    pk <- prereq$key
    if (identical(pk, state$original_flag_key) || pk %in% state$prereq_stack) {
      stop("circular prerequisite", call. = FALSE)
    }
    pflag <- get_flag(pk)
    if (is.null(pflag) || isTRUE(pflag$deleted)) {
      return(list(kind = "PREREQUISITE_FAILED", prerequisiteKey = pk))
    }
    pres <- .ld_eval_flag(pflag, context, state, get_flag, get_segment)
    if (!isTRUE(pflag$on) || !identical(as.integer(pres$variation_index), as.integer(prereq$variation))) {
      return(list(kind = "PREREQUISITE_FAILED", prerequisiteKey = pk))
    }
  }

  if (length(state$prereq_stack) > 0L) {
    state$prereq_stack <- state$prereq_stack[-length(state$prereq_stack)]
  }
  NULL
}

.ld_check_targets <- function(flag, context) {
  user_targets <- flag$targets %||% list()
  context_targets <- flag$contextTargets %||% list()

  if (length(context_targets) == 0L) {
    if (length(user_targets) == 0L) return(NULL)
    user_ctx <- context_get_individual(context, "user")
    if (is.null(user_ctx)) return(NULL)
    key <- user_ctx$key
    for (t in user_targets) {
      vals <- unlist(t$values %||% list(), use.names = FALSE)
      if (key %in% vals) {
        return(.ld_get_variation(flag, t$variation, list(kind = "TARGET_MATCH")))
      }
    }
    return(NULL)
  }

  for (t in context_targets) {
    kind <- t$contextKind %||% "user"
    var <- t$variation
    actual <- context_get_individual(context, kind)
    if (is.null(actual)) next
    key <- actual$key
    if (identical(kind, "user")) {
      for (ut in user_targets) {
        if (identical(as.integer(ut$variation), as.integer(var))) {
          vals <- unlist(ut$values %||% list(), use.names = FALSE)
          if (key %in% vals) {
            return(.ld_get_variation(flag, var, list(kind = "TARGET_MATCH")))
          }
          break
        }
      }
      next
    }
    vals <- unlist(t$values %||% list(), use.names = FALSE)
    if (key %in% vals) {
      return(.ld_get_variation(flag, var, list(kind = "TARGET_MATCH")))
    }
  }
  NULL
}

.ld_value_for_vr <- function(flag, vr, context, reason) {
  if (is.null(vr)) {
    return(evaluation_detail(NULL, NULL, list(kind = "ERROR", errorKind = "MALFORMED_FLAG")))
  }
  # Rule may have variation at top level or nested
  if (!is.null(vr$variation) && is.null(vr$rollout)) {
    return(.ld_get_variation(flag, vr$variation, reason))
  }
  idx_exp <- .ld_variation_index_for_context(flag, vr, context)
  idx <- idx_exp$index
  if (is.null(idx)) {
    return(evaluation_detail(NULL, NULL, list(kind = "ERROR", errorKind = "MALFORMED_FLAG")))
  }
  if (isTRUE(idx_exp$in_experiment)) reason$inExperiment <- TRUE
  .ld_get_variation(flag, idx, reason)
}

.ld_variation_index_for_context <- function(flag, vr, context) {
  if (!is.null(vr$variation) && is.null(vr$rollout)) {
    return(list(index = as.integer(vr$variation), in_experiment = FALSE))
  }
  rollout <- vr$rollout
  if (is.null(rollout)) return(list(index = NULL, in_experiment = FALSE))
  variations <- rollout$variations %||% list()
  if (length(variations) == 0L) return(list(index = NULL, in_experiment = FALSE))

  is_experiment <- identical(rollout$kind, "experiment") || isTRUE(rollout$experiment)
  bucket_by <- if (is_experiment) NULL else rollout$bucketBy
  bucket <- .ld_bucket_context(
    rollout$seed, context, rollout$contextKind, flag$key, flag$salt %||% "", bucket_by
  )
  in_exp <- is_experiment && bucket >= 0

  sum <- 0
  for (wv in variations) {
    sum <- sum + (as.numeric(wv$weight %||% 0) / 100000)
    if (bucket < sum) {
      part <- in_exp && !isTRUE(wv$untracked)
      return(list(index = as.integer(wv$variation), in_experiment = part))
    }
  }
  last <- variations[[length(variations)]]
  list(
    index = as.integer(last$variation),
    in_experiment = in_exp && !isTRUE(last$untracked)
  )
}

.ld_bucket_context <- function(seed, context, context_kind, key, salt, bucket_by) {
  match_ctx <- context_get_individual(context, context_kind %||% "user")
  if (is.null(match_ctx)) return(-1)
  clause_value <- if (is.null(bucket_by)) {
    match_ctx$key
  } else {
    .ld_attr_value(match_ctx, bucket_by)
  }
  if (is.null(clause_value)) return(0)
  bucketable <- .ld_bucketable_string(clause_value)
  if (is.null(bucketable)) return(0)
  prefix <- if (!is.null(seed)) as.character(seed) else paste(key, salt, sep = ".")
  hash_key <- paste(prefix, bucketable, sep = ".")
  hex <- digest::digest(hash_key, algo = "sha1", serialize = FALSE)
  # First 15 hex digits as integer / (2^60 - 1), matching Python __LONG_SCALE__
  # Avoid strtoi() which overflows signed 32-bit for high hex digits.
  h <- substr(hex, 1L, 15L)
  hi <- as.numeric(as.hexmode(substr(h, 1L, 7L)))
  lo_a <- as.numeric(as.hexmode(substr(h, 8L, 11L)))
  lo_b <- as.numeric(as.hexmode(substr(h, 12L, 15L)))
  hash_val <- hi * 16^8 + lo_a * 16^4 + lo_b
  hash_val / .ld_LONG_SCALE
}

.ld_bucketable_string <- function(u) {
  if (is.null(u) || length(u) != 1L) return(NULL)
  if (is.logical(u)) return(NULL)
  if (is.character(u)) return(u)
  if (is.numeric(u) && !is.logical(u)) {
    if (is.na(u) || u != floor(u)) return(NULL) # non-integer numerics are not bucketable
    # Match other SDKs: integer-valued numbers stringify without a fractional part
    return(format(as.integer(u), scientific = FALSE, trim = TRUE))
  }
  NULL
}

.ld_parse_attr_ref <- function(attr, literal = FALSE) {
  if (is.null(attr)) return(list(error = "rule clause did not specify an attribute"))
  if (is.list(attr) && !is.null(attr$path)) attr <- attr$path
  attr <- as.character(attr)
  if (length(attr) != 1L) return(list(error = "invalid attribute reference"))
  if (!nzchar(attr)) return(list(error = "invalid attribute reference: empty"))
  # Legacy clauses (no contextKind): always a literal attribute name, even with `/`.
  if (isTRUE(literal) || !startsWith(attr, "/")) {
    return(list(parts = attr))
  }
  raw <- substring(attr, 2L)
  parts <- strsplit(raw, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0L || any(!nzchar(parts))) {
    return(list(error = "invalid attribute reference: empty path component"))
  }
  # Validate ~ escapes: only ~0 and ~1 allowed
  for (p in parts) {
    if (grepl("~([^01]|$)", p)) {
      return(list(error = "invalid attribute reference: bad escape"))
    }
  }
  parts <- gsub("~1", "/", parts, fixed = TRUE)
  parts <- gsub("~0", "~", parts, fixed = TRUE)
  list(parts = parts)
}

.ld_attr_value <- function(context, attr, literal = FALSE) {
  parsed <- .ld_parse_attr_ref(attr, literal = literal)
  if (!is.null(parsed$error)) stop(parsed$error, call. = FALSE)
  parts <- parsed$parts
  if (length(parts) == 0L) return(NULL)
  value <- context_get(context, parts[[1]])
  if (length(parts) == 1L) return(value)
  for (i in 2:length(parts)) {
    if (!is.list(value) || is.null(names(value))) return(NULL)
    value <- value[[parts[[i]]]]
  }
  value
}

.ld_rule_matches <- function(rule, context, state, get_segment) {
  clauses <- rule$clauses %||% list()
  for (clause in clauses) {
    if (!.ld_clause_matches(clause, context, state, get_segment)) return(FALSE)
  }
  TRUE
}

.ld_clause_matches <- function(clause, context, state, get_segment) {
  op <- clause$op %||% ""
  if (identical(op, "segmentMatch")) {
    vals <- clause$values %||% list()
    matched <- FALSE
    for (seg_key in vals) {
      segment <- get_segment(as.character(seg_key))
      if (is.null(segment) || isTRUE(segment$deleted)) next
      if (.ld_segment_matches(segment, context, state, get_segment)) {
        matched <- TRUE
        break
      }
    }
    return(.ld_maybe_negate(clause, matched))
  }

  attr <- clause$attribute
  # Absence of contextKind → legacy literal attribute name (even if starts with /)
  literal <- is.null(clause$contextKind)
  parsed <- .ld_parse_attr_ref(attr, literal = literal)
  if (!is.null(parsed$error)) stop(parsed$error, call. = FALSE)
  parts <- parsed$parts
  if (length(parts) == 1L && identical(parts[[1]], "kind")) {
    return(.ld_maybe_negate(clause, .ld_match_clause_by_kind(clause, context)))
  }

  kind <- clause$contextKind %||% "user"
  actual <- context_get_individual(context, kind)
  if (is.null(actual)) return(FALSE)
  ctx_value <- .ld_attr_value(actual, attr, literal = literal)
  if (is.null(ctx_value)) return(FALSE)

  if (is.list(ctx_value) && is.null(names(ctx_value))) {
    # array
    for (v in ctx_value) {
      if (.ld_match_clause_values(clause, v)) return(.ld_maybe_negate(clause, TRUE))
    }
    return(.ld_maybe_negate(clause, FALSE))
  }
  .ld_maybe_negate(clause, .ld_match_clause_values(clause, ctx_value))
}

.ld_match_clause_values <- function(clause, ctx_value) {
  for (v in clause$values %||% list()) {
    if (.ld_match_op(clause$op, ctx_value, v)) return(TRUE)
  }
  FALSE
}

.ld_match_clause_by_kind <- function(clause, context) {
  n <- context_individual_count(context)
  for (i in seq_len(n) - 1L) {
    c <- context_get_individual(context, i)
    if (!is.null(c) && .ld_match_clause_values(clause, c$kind)) return(TRUE)
  }
  FALSE
}

.ld_maybe_negate <- function(clause, val) {
  if (isTRUE(clause$negate)) !val else val
}

.ld_segment_matches <- function(segment, context, state, get_segment) {
  if (segment$key %in% state$segment_stack) stop("circular segment", call. = FALSE)
  if (isTRUE(segment$unbounded)) {
    # Big segments out of scope — do not match via store
    return(FALSE)
  }
  .ld_simple_segment_match(segment, context, state, get_segment, TRUE)
}

.ld_simple_segment_match <- function(segment, context, state, get_segment, use_inc_exc) {
  if (isTRUE(use_inc_exc)) {
    included <- unlist(segment$included %||% list(), use.names = FALSE)
    if (.ld_key_in_list(context, NULL, included)) return(TRUE)
    for (t in segment$includedContexts %||% list()) {
      vals <- unlist(t$values %||% list(), use.names = FALSE)
      if (.ld_key_in_list(context, t$contextKind, vals)) return(TRUE)
    }
    excluded <- unlist(segment$excluded %||% list(), use.names = FALSE)
    if (.ld_key_in_list(context, NULL, excluded)) return(FALSE)
    for (t in segment$excludedContexts %||% list()) {
      vals <- unlist(t$values %||% list(), use.names = FALSE)
      if (.ld_key_in_list(context, t$contextKind, vals)) return(FALSE)
    }
  }

  rules <- segment$rules %||% list()
  if (length(rules) == 0L) return(FALSE)

  state$segment_stack <- c(state$segment_stack, segment$key)
  matched <- FALSE
  for (rule in rules) {
    if (.ld_segment_rule_matches(rule, context, state, get_segment, segment$key, segment$salt %||% "")) {
      matched <- TRUE
      break
    }
  }
  state$segment_stack <- state$segment_stack[-length(state$segment_stack)]
  matched
}

.ld_key_in_list <- function(context, context_kind, keys) {
  if (is.null(keys) || length(keys) == 0L) return(FALSE)
  match_ctx <- context_get_individual(context, context_kind %||% "user")
  !is.null(match_ctx) && match_ctx$key %in% keys
}

.ld_segment_rule_matches <- function(rule, context, state, get_segment, segment_key, salt) {
  for (clause in rule$clauses %||% list()) {
    if (!.ld_clause_matches(clause, context, state, get_segment)) return(FALSE)
  }
  if (is.null(rule$weight)) return(TRUE)
  bucket <- .ld_bucket_context(NULL, context, rule$rolloutContextKind, segment_key, salt, rule$bucketBy)
  weight <- as.numeric(rule$weight) / 100000
  bucket < weight
}
