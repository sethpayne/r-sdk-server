#' Analytics event buffer, summarizer, and formatting (inline context schema 4)
#'
#' Runs inside the worker. Parent processes send evaluation records (`Events`
#' IPC messages), identify and custom events; this class turns them into the
#' output events posted to `/bulk` (feature, debug, index, identify, custom,
#' summary), following python-server-sdk `EventDispatcherBase` semantics.
#'
#' @keywords internal
NULL

#' @export
EventBuffer <- R6::R6Class(
  "EventBuffer",
  public = list(
    initialize = function(capacity = 10000L, all_private = FALSE, private_attrs = character(),
                          context_keys_capacity = 1000L, omit_anonymous_contexts = FALSE) {
      private$capacity_ <- as.integer(capacity)
      private$all_private_ <- isTRUE(all_private)
      private$private_attrs_ <- as.character(unlist(private_attrs, use.names = FALSE))
      private$events_ <- list()
      private$context_keys_capacity_ <- max(1L, as.integer(context_keys_capacity %||% 1000L))
      private$omit_anonymous_ <- isTRUE(omit_anonymous_contexts)
      self$reset_context_keys()
      private$reset_summary()
      invisible(self)
    },

    size = function() length(private$events_),

    dropped_count = function() private$dropped_,

    set_server_time = function(ms) {
      if (is.numeric(ms) && length(ms) == 1L && !is.na(ms)) private$last_server_time_ <- ms
      invisible(self)
    },

    reset_context_keys = function() {
      private$seen_ <- new.env(parent = emptyenv(), hash = TRUE)
      private$seen_order_ <- character()
      invisible(self)
    },

    # --- Low-level adders (already-formatted kinds) ----------------------------

    add_index = function(context) {
      private$push(list(
        kind = "index",
        creationDate = private$now_ms(),
        context = private$format_context(context, redact_anonymous = FALSE)
      ))
    },

    add_identify = function(context, creation_date = NULL) {
      private$push(list(
        kind = "identify",
        creationDate = creation_date %||% private$now_ms(),
        context = private$format_context(context, redact_anonymous = FALSE)
      ))
    },

    add_custom = function(event_name, context, data = NULL, metric_value = NULL,
                          creation_date = NULL) {
      ev <- list(
        kind = "custom",
        key = event_name,
        creationDate = creation_date %||% private$now_ms(),
        context = private$format_context(context, redact_anonymous = TRUE)
      )
      if (!is.null(data)) ev$data <- data
      if (!is.null(metric_value)) ev$metricValue <- metric_value
      private$push(ev)
    },

    add_feature = function(flag_key, context, value, default, version = NULL,
                           variation = NULL, track_events = FALSE, reason = NULL,
                           prereq_of = NULL, creation_date = NULL, debug = FALSE) {
      ev <- list(
        kind = if (isTRUE(debug)) "debug" else "feature",
        creationDate = creation_date %||% private$now_ms(),
        key = flag_key,
        value = value,
        default = default
      )
      if (is.null(value)) ev["value"] <- list(NULL)
      if (is.null(default)) ev["default"] <- list(NULL)
      if (!is.null(version)) ev$version <- version
      if (!is.null(variation)) ev$variation <- variation
      if (!is.null(reason)) ev$reason <- reason
      if (!is.null(prereq_of)) ev$prereqOf <- prereq_of
      ev$context <- private$format_context(context, redact_anonymous = !isTRUE(debug))
      private$push(ev)
    },

    add_summary = function(start_ms, end_ms, counters) {
      private$push(list(
        kind = "summary",
        startDate = start_ms,
        endDate = end_ms,
        features = counters
      ))
    },

    # --- Event processing (input records -> output events) -----------------------

    #' @param rec Evaluation record built by the parent (`.ld_eval_event_record()`).
    process_eval = function(rec) {
      ctx_list <- rec$context
      ctx <- .ld_context_or_null(ctx_list)
      if (is.null(ctx)) return(invisible(self))
      now <- private$now_ms()
      ts <- rec$creationDate %||% now
      if (!isTRUE(rec$excludeFromSummaries)) private$summarize(rec, ctx, ts)
      private$maybe_index(ctx, ts, is_identify = FALSE)
      if (isTRUE(rec$trackEvents)) {
        self$add_feature(rec$key, ctx_list, rec$value, rec$default,
                         version = rec$version, variation = rec$variation,
                         reason = rec$reason, prereq_of = rec$prereqOf, creation_date = ts)
      }
      du <- rec$debugEventsUntilDate
      # Debugging also stops once LaunchDarkly's clock (from /bulk responses)
      # has passed the deadline, even if the local clock lags.
      if (is.numeric(du) && length(du) == 1L && !is.na(du) && du > now &&
          du > private$last_server_time_) {
        self$add_feature(rec$key, ctx_list, rec$value, rec$default,
                         version = rec$version, variation = rec$variation,
                         reason = rec$reason, prereq_of = rec$prereqOf, creation_date = ts,
                         debug = TRUE)
      }
      invisible(self)
    },

    process_identify = function(context_list, creation_date = NULL) {
      ctx <- .ld_context_or_null(context_list)
      if (is.null(ctx)) return(invisible(self))
      if (private$omit_anonymous_) {
        ctx <- context_without_anonymous(ctx)
        if (!context_valid(ctx)) return(invisible(self))
        context_list <- context_to_list(ctx)
      }
      private$maybe_index(ctx, creation_date, is_identify = TRUE)
      self$add_identify(context_list, creation_date = creation_date)
    },

    process_custom = function(event_name, context_list, data = NULL, metric_value = NULL,
                              creation_date = NULL) {
      ctx <- .ld_context_or_null(context_list)
      if (is.null(ctx)) return(invisible(self))
      ts <- creation_date %||% private$now_ms()
      private$maybe_index(ctx, ts, is_identify = FALSE)
      self$add_custom(event_name, context_list, data, metric_value, creation_date = ts)
    },

    #' Output events accumulated so far (summary last), then reset.
    payload = function() {
      out <- private$events_
      private$events_ <- list()
      summary <- private$summary_event()
      if (!is.null(summary)) out[[length(out) + 1L]] <- summary
      private$reset_summary()
      out
    },

    drain = function() {
      out <- private$events_
      private$events_ <- list()
      out
    }
  ),
  private = list(
    capacity_ = 10000L,
    all_private_ = FALSE,
    private_attrs_ = character(),
    events_ = NULL,
    dropped_ = 0L,
    last_server_time_ = 0,
    seen_ = NULL,
    seen_order_ = NULL,
    context_keys_capacity_ = 1000L,
    omit_anonymous_ = FALSE,
    summary_flags_ = NULL,
    summary_start_ = 0,
    summary_end_ = 0,
    now_ms = function() round(as.numeric(Sys.time()) * 1000),
    push = function(ev) {
      if (length(private$events_) >= private$capacity_) {
        private$dropped_ <- private$dropped_ + 1L
        return(invisible(FALSE))
      }
      private$events_[[length(private$events_) + 1L]] <- ev
      invisible(TRUE)
    },
    format_context = function(context_list, redact_anonymous) {
      .ld_format_event_context(context_list, private$all_private_, private$private_attrs_,
                               redact_anonymous = redact_anonymous)
    },
    maybe_index = function(ctx, ts, is_identify) {
      if (private$omit_anonymous_) {
        ctx <- context_without_anonymous(ctx)
        if (!context_valid(ctx)) return(invisible(NULL))
      }
      fqk <- context_fully_qualified_key(ctx)
      if (!nzchar(fqk)) return(invisible(NULL))
      seen <- exists(fqk, envir = private$seen_, inherits = FALSE)
      if (!seen) {
        assign(fqk, TRUE, envir = private$seen_)
        private$seen_order_ <- c(private$seen_order_, fqk)
        if (length(private$seen_order_) > private$context_keys_capacity_) {
          old <- private$seen_order_[[1L]]
          private$seen_order_ <- private$seen_order_[-1L]
          rm(list = old, envir = private$seen_)
        }
      }
      if (seen || isTRUE(is_identify)) return(invisible(NULL))
      private$push(list(
        kind = "index",
        creationDate = ts %||% private$now_ms(),
        context = private$format_context(context_to_list(ctx), redact_anonymous = FALSE)
      ))
    },
    reset_summary = function() {
      private$summary_flags_ <- list()
      private$summary_start_ <- 0
      private$summary_end_ <- 0
    },
    summarize = function(rec, ctx, ts) {
      key <- rec$key
      fd <- private$summary_flags_[[key]]
      if (is.null(fd)) {
        fd <- list(default = rec$default, kinds = character(), counters = list())
        if (is.null(rec$default)) fd["default"] <- list(NULL)
      }
      for (i in seq_len(context_individual_count(ctx)) - 1L) {
        c <- context_get_individual(ctx, i)
        if (!is.null(c)) fd$kinds <- union(fd$kinds, c$kind)
      }
      ckey <- paste(rec$variation %||% "null", rec$version %||% "unknown", sep = "|")
      cnt <- fd$counters[[ckey]]
      if (is.null(cnt)) {
        cnt <- list(count = 1L, value = rec$value, variation = rec$variation, version = rec$version)
        if (is.null(rec$value)) cnt["value"] <- list(NULL)
      } else {
        cnt$count <- cnt$count + 1L
      }
      fd$counters[[ckey]] <- cnt
      private$summary_flags_[[key]] <- fd
      if (private$summary_start_ == 0 || ts < private$summary_start_) private$summary_start_ <- ts
      if (ts > private$summary_end_) private$summary_end_ <- ts
    },
    summary_event = function() {
      if (length(private$summary_flags_) == 0L) return(NULL)
      features <- list()
      for (key in names(private$summary_flags_)) {
        fd <- private$summary_flags_[[key]]
        counters <- unname(lapply(fd$counters, function(cnt) {
          out <- list(count = cnt$count, value = cnt$value)
          if (is.null(cnt$value)) out["value"] <- list(NULL)
          if (!is.null(cnt$variation)) out$variation <- cnt$variation
          if (is.null(cnt$version)) out$unknown <- TRUE else out$version <- cnt$version
          out
        }))
        fo <- list(default = fd$default, contextKinds = as.list(fd$kinds), counters = counters)
        if (is.null(fd$default)) fo["default"] <- list(NULL)
        features[[key]] <- fo
      }
      list(
        kind = "summary",
        startDate = private$summary_start_,
        endDate = private$summary_end_,
        features = features
      )
    }
  )
)

.ld_context_or_null <- function(context_list) {
  if (!is.list(context_list) || length(context_list) == 0L) return(NULL)
  ctx <- tryCatch(context_from_list(context_list), error = function(e) NULL)
  if (is.null(ctx) || !context_valid(ctx)) return(NULL)
  ctx
}

#' Redact a JSON-shaped context list for analytics output
#'
#' Applies config-level and context-level (`_meta.privateAttributes`) private
#' attribute references, including nested `/a/b` paths. Removed attributes are
#' listed in `_meta.redactedAttributes`; `key`, `kind` and `anonymous` are
#' never redacted.
#'
#' @param context_list Output of [context_to_list()].
#' @param all_private Redact every attribute except key/kind/anonymous.
#' @param private_attrs Character vector of attribute references.
#' @param redact_anonymous Redact all attributes of anonymous contexts.
#' @keywords internal
.ld_format_event_context <- function(context_list, all_private = FALSE,
                                     private_attrs = character(), redact_anonymous = FALSE) {
  if (!is.list(context_list)) return(context_list)
  global_refs <- .ld_parse_private_refs(private_attrs)
  if (identical(context_list$kind, "multi")) {
    out <- list(kind = "multi")
    for (nm in setdiff(names(context_list), "kind")) {
      out[[nm]] <- .ld_format_event_context_single(
        context_list[[nm]], NULL, all_private, global_refs, redact_anonymous
      )
    }
    return(out)
  }
  .ld_format_event_context_single(
    context_list, context_list$kind %||% "user", all_private, global_refs, redact_anonymous
  )
}

.ld_parse_private_refs <- function(refs) {
  refs <- unlist(refs, use.names = FALSE)
  out <- list()
  for (r in refs) {
    if (!is.character(r) || length(r) != 1L || is.na(r) || !nzchar(r)) next
    parsed <- .ld_parse_attr_ref(r)
    if (!is.null(parsed$error)) next
    out[[length(out) + 1L]] <- list(path = r, parts = parsed$parts)
  }
  out
}

.ld_literal_ref_path <- function(name) {
  if (!startsWith(name, "/")) return(name)
  paste0("/", gsub("/", "~1", gsub("~", "~0", name, fixed = TRUE), fixed = TRUE))
}

.ld_format_event_context_single <- function(obj, kind, all_private, global_refs, redact_anonymous) {
  out <- list(key = obj$key %||% "")
  if (!is.null(kind)) out$kind <- kind
  anonymous <- isTRUE(obj$anonymous)
  if (anonymous) out$anonymous <- TRUE
  refs <- c(global_refs, .ld_parse_private_refs(obj[["_meta"]]$privateAttributes))
  redact_all <- isTRUE(all_private) || (anonymous && isTRUE(redact_anonymous))
  redacted <- new.env(parent = emptyenv())
  redacted$paths <- character()
  for (nm in setdiff(names(obj), c("key", "kind", "anonymous", "_meta"))) {
    whole <- redact_all || any(vapply(refs, function(r) {
      length(r$parts) == 1L && identical(r$parts[[1L]], nm)
    }, logical(1)))
    if (whole) {
      redacted$paths <- c(redacted$paths, .ld_literal_ref_path(nm))
      next
    }
    out[nm] <- list(.ld_redact_json_value(nm, obj[[nm]], refs, redacted))
  }
  if (length(redacted$paths)) {
    out[["_meta"]] <- list(redactedAttributes = as.list(redacted$paths))
  }
  out
}

.ld_redact_json_value <- function(path, value, refs, redacted) {
  if (!is.list(value) || length(value) == 0L || is.null(names(value))) return(value)
  ret <- list()
  depth <- length(path) + 1L
  for (k in names(value)) {
    hit <- NULL
    for (r in refs) {
      if (length(r$parts) != depth) next
      if (!identical(r$parts[[depth]], k)) next
      if (identical(as.character(r$parts[seq_len(depth - 1L)]), as.character(path))) {
        hit <- r
        break
      }
    }
    if (!is.null(hit)) {
      redacted$paths <- c(redacted$paths, hit$path)
      next
    }
    ret[k] <- list(.ld_redact_json_value(c(path, k), value[[k]], refs, redacted))
  }
  if (length(ret) == 0L) ret <- structure(list(), names = character())
  ret
}

#' Is an evaluation part of an experiment (python `EventFactory.is_experiment`)
#' @keywords internal
.ld_is_experiment <- function(flag, reason) {
  if (is.null(reason) || is.null(flag)) return(FALSE)
  if (isTRUE(reason$inExperiment)) return(TRUE)
  kind <- reason$kind %||% ""
  if (identical(kind, "RULE_MATCH")) {
    ri <- reason$ruleIndex
    rules <- flag$rules %||% list()
    return(!is.null(ri) && length(ri) == 1L && !is.na(ri) && ri >= 0L && ri < length(rules) &&
      isTRUE(rules[[ri + 1L]]$trackEvents))
  }
  if (identical(kind, "FALLTHROUGH")) return(isTRUE(flag$trackEventsFallthrough))
  FALSE
}

#' Build an evaluation event record for IPC (python `EventFactory`)
#'
#' @param flag Flag list or `NULL` for unknown flags.
#' @param detail `ld_evaluation_detail`.
#' @param with_reasons Include the reason (variation_detail).
#' @keywords internal
.ld_eval_event_record <- function(key, context, flag, detail, default, with_reasons,
                                  prereq_of = NULL, timestamp = NULL) {
  experiment <- !is.null(flag) && .ld_is_experiment(flag, detail$reason)
  rec <- list(
    kind = "eval",
    creationDate = timestamp %||% round(as.numeric(Sys.time()) * 1000),
    key = key,
    value = detail$value,
    default = default,
    context = context_to_list(context)
  )
  if (is.null(detail$value)) rec["value"] <- list(NULL)
  if (is.null(default)) rec["default"] <- list(NULL)
  if (!is.null(flag)) {
    if (!is.null(flag$version)) rec$version <- flag$version
    rec$trackEvents <- isTRUE(flag$trackEvents) || experiment
    if (!is.null(flag$debugEventsUntilDate)) rec$debugEventsUntilDate <- flag$debugEventsUntilDate
    if (isTRUE(flag$excludeFromSummaries)) rec$excludeFromSummaries <- TRUE
  } else {
    rec$trackEvents <- FALSE
  }
  if (!is.null(detail$variation_index)) rec$variation <- detail$variation_index
  if ((isTRUE(with_reasons) || experiment) && !is.null(detail$reason)) rec$reason <- detail$reason
  if (!is.null(prereq_of)) rec$prereqOf <- prereq_of
  rec
}

.ld_new_payload_id <- function() {
  hex <- sample(c(0:9, letters[1:6]), 32L, replace = TRUE)
  hex[13L] <- "4"
  hex[17L] <- sample(c("8", "9", "a", "b"), 1L)
  h <- paste(hex, collapse = "")
  paste(substr(h, 1, 8), substr(h, 9, 12), substr(h, 13, 16), substr(h, 17, 20),
        substr(h, 21, 32), sep = "-")
}

# 400, 408, 429 and all 5xx (and network errors) are recoverable; other 4xx disable sending.
.ld_http_error_recoverable <- function(status) {
  if (is.null(status) || is.na(status)) return(TRUE)
  if (status >= 400 && status < 500) return(status %in% c(400L, 408L, 429L))
  TRUE
}

# Server clock from the response `Date` header, in epoch milliseconds.
.ld_response_date_ms <- function(resp) {
  d <- httr2::resp_header(resp, "Date")
  if (is.null(d)) return(NULL)
  t <- tryCatch(curl::parse_date(d), error = function(e) NA)
  # curl's parser overflows to negative values past 2038 on some platforms.
  if (length(t) != 1L || is.na(t) || as.numeric(t) <= 0) return(NULL)
  as.numeric(t) * 1000
}

#' POST events to LaunchDarkly events URI
#'
#' Retries once after `retry_delay` seconds on a recoverable failure, reusing
#' the same `X-LaunchDarkly-Payload-ID`.
#'
#' @return List with `ok`, `status`, `count`, `error`, and `unrecoverable`
#'   (TRUE when the server rejected the key, e.g. 401/403).
#' @keywords internal
ld_post_events <- function(config, events, retry_delay = 1) {
  if (!isTRUE(config$send_events) || length(events) == 0L) {
    return(list(ok = TRUE, count = 0L))
  }
  base <- sub("/$", "", config$events_uri %||% "https://events.launchdarkly.com")
  url <- paste0(base, "/bulk")
  payload_id <- .ld_new_payload_id()
  body <- charToRaw(enc2utf8(as.character(
    jsonlite::toJSON(events, auto_unbox = TRUE, null = "null", digits = NA)
  )))
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = config$sdk_key,
      `User-Agent` = LD_USER_AGENT,
      `Content-Type` = "application/json",
      `X-LaunchDarkly-Event-Schema` = "4",
      `X-LaunchDarkly-Payload-ID` = payload_id
    ) |>
    httr2::req_body_raw(body, "application/json") |>
    httr2::req_timeout(config$read_timeout %||% 15) |>
    httr2::req_error(is_error = function(resp) FALSE)

  tags <- .ld_tags_header_value(config)
  if (!is.null(tags)) {
    req <- httr2::req_headers(req, `X-LaunchDarkly-Tags` = tags)
  }

  last <- NULL
  for (attempt in 1:2) {
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      last <- list(ok = FALSE, error = conditionMessage(resp), count = length(events),
                   unrecoverable = FALSE)
    } else {
      status <- httr2::resp_status(resp)
      if (status < 300) {
        return(list(ok = TRUE, status = status, count = length(events), payload_id = payload_id,
                    server_time = .ld_response_date_ms(resp)))
      }
      recoverable <- .ld_http_error_recoverable(status)
      last <- list(ok = FALSE, status = status, error = sprintf("events HTTP %s", status),
                   count = length(events), unrecoverable = !recoverable)
      if (!recoverable) return(last)
    }
    if (attempt == 1L && retry_delay > 0) Sys.sleep(retry_delay)
  }
  last
}
