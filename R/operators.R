#' Clause operators (parity with python-server-sdk operators.py)
#'
#' @keywords internal
NULL

.ld_is_number <- function(x) {
  is.numeric(x) && !is.logical(x) && length(x) == 1L && !is.na(x)
}

# JSON numbers arrive from jsonlite as integer or double depending on their text
# ("30" vs "30.0"), while R users usually write doubles; compare numerically.
.ld_op_in <- function(ctx, clause, prep = NULL) {
  if (.ld_is_number(ctx) && .ld_is_number(clause)) {
    return(as.numeric(ctx) == as.numeric(clause))
  }
  if (is.logical(ctx) != is.logical(clause)) return(FALSE)
  if (is.character(ctx) && is.character(clause)) {
    return(length(ctx) == 1L && length(clause) == 1L && !is.na(ctx) && !is.na(clause) &&
      ctx == clause)
  }
  identical(unname(ctx), unname(clause))
}

.ld_op_starts_with <- function(ctx, clause, prep = NULL) {
  is.character(ctx) && is.character(clause) && startsWith(ctx, clause)
}

.ld_op_ends_with <- function(ctx, clause, prep = NULL) {
  is.character(ctx) && is.character(clause) && endsWith(ctx, clause)
}

.ld_op_contains <- function(ctx, clause, prep = NULL) {
  is.character(ctx) && is.character(clause) && grepl(clause, ctx, fixed = TRUE)
}

.ld_op_matches <- function(ctx, clause, prep = NULL) {
  if (!is.character(ctx) || !is.character(clause)) return(FALSE)
  tryCatch(grepl(clause, ctx, perl = TRUE), error = function(e) FALSE)
}

.ld_op_lt <- function(ctx, clause, prep = NULL) {
  .ld_is_number(ctx) && .ld_is_number(clause) && as.numeric(ctx) < as.numeric(clause)
}
.ld_op_lte <- function(ctx, clause, prep = NULL) {
  .ld_is_number(ctx) && .ld_is_number(clause) && as.numeric(ctx) <= as.numeric(clause)
}
.ld_op_gt <- function(ctx, clause, prep = NULL) {
  .ld_is_number(ctx) && .ld_is_number(clause) && as.numeric(ctx) > as.numeric(clause)
}
.ld_op_gte <- function(ctx, clause, prep = NULL) {
  .ld_is_number(ctx) && .ld_is_number(clause) && as.numeric(ctx) >= as.numeric(clause)
}

# Time ops: accept epoch millis (numeric) or RFC3339 / ISO-8601 strings
.ld_parse_time_ms <- function(x) {
  if (.ld_is_number(x)) return(as.numeric(x))
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) return(NULL)
  # RFC3339 requires date *and* time; date-only / bare times are invalid
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", x)) return(NULL)
  # Normalize trailing ±HH:MM offset to ±HHMM for R %z
  x_norm <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  has_tz <- grepl("(Z|[+-][0-9]{4})$", x_norm)
  formats <- if (has_tz) {
    c("%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%dT%H:%M:%OSZ")
  } else if (!grepl("[Zz]|[+-][0-9]{2}", x)) {
    # No timezone present — interpret as UTC wall time
    c("%Y-%m-%dT%H:%M:%OS")
  } else {
    # Timezone-looking suffix we couldn't normalize (e.g. bad offset) — reject
    return(NULL)
  }
  for (fmt in formats) {
    t <- suppressWarnings(as.POSIXct(x_norm, tz = "UTC", format = fmt))
    if (!is.na(t)) return(as.numeric(t) * 1000)
  }
  NULL
}

.ld_op_before <- function(ctx, clause, prep = NULL) {
  a <- .ld_parse_time_ms(ctx); b <- .ld_parse_time_ms(clause)
  !is.null(a) && !is.null(b) && a < b
}
.ld_op_after <- function(ctx, clause, prep = NULL) {
  a <- .ld_parse_time_ms(ctx); b <- .ld_parse_time_ms(clause)
  !is.null(a) && !is.null(b) && a > b
}

# Semver: major[.minor[.patch]] with optional prerelease (no build metadata).
# Missing minor/patch default to 0 (harness treats "2" == "2.0.0").
# Leading zeros and >3 numeric core parts are invalid (bad-syntax suite).
.ld_parse_semver <- function(x) {
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) return(NULL)
  m <- regmatches(
    x,
    regexec("^([0-9]+)(?:\\.([0-9]+))?(?:\\.([0-9]+))?(?:-([0-9A-Za-z.-]+))?(?:\\+[0-9A-Za-z.-]+)?$", x)
  )[[1]]
  if (length(m) < 2L || !nzchar(m[2] %||% "")) return(NULL)
  major <- m[2]
  minor <- if (length(m) >= 3L && nzchar(m[3] %||% "")) m[3] else "0"
  patch <- if (length(m) >= 4L && nzchar(m[4] %||% "")) m[4] else "0"
  for (part in c(major, minor, patch)) {
    if (nchar(part) > 1L && startsWith(part, "0")) return(NULL)
  }
  pre <- if (length(m) >= 5L && nzchar(m[5] %||% "")) strsplit(m[5], ".", fixed = TRUE)[[1]] else character()
  if (any(!nzchar(pre))) return(NULL)
  list(core = as.integer(c(major, minor, patch)), pre = pre)
}

.ld_semver_cmp <- function(a, b) {
  aa <- .ld_parse_semver(a); bb <- .ld_parse_semver(b)
  if (is.null(aa) || is.null(bb)) return(NA_integer_)
  for (i in 1:3) {
    if (aa$core[i] < bb$core[i]) return(-1L)
    if (aa$core[i] > bb$core[i]) return(1L)
  }
  # No prerelease > any prerelease
  if (length(aa$pre) == 0L && length(bb$pre) == 0L) return(0L)
  if (length(aa$pre) == 0L) return(1L)
  if (length(bb$pre) == 0L) return(-1L)
  n <- max(length(aa$pre), length(bb$pre))
  for (i in seq_len(n)) {
    if (i > length(aa$pre)) return(-1L)
    if (i > length(bb$pre)) return(1L)
    pa <- aa$pre[[i]]; pb <- bb$pre[[i]]
    a_num <- grepl("^[0-9]+$", pa); b_num <- grepl("^[0-9]+$", pb)
    if (a_num && b_num) {
      ai <- as.integer(pa); bi <- as.integer(pb)
      if (ai < bi) return(-1L)
      if (ai > bi) return(1L)
    } else if (a_num && !b_num) {
      return(-1L)
    } else if (!a_num && b_num) {
      return(1L)
    } else {
      if (pa < pb) return(-1L)
      if (pa > pb) return(1L)
    }
  }
  0L
}

.ld_op_sem_eq <- function(ctx, clause, prep = NULL) identical(.ld_semver_cmp(ctx, clause), 0L)
.ld_op_sem_lt <- function(ctx, clause, prep = NULL) identical(.ld_semver_cmp(ctx, clause), -1L)
.ld_op_sem_gt <- function(ctx, clause, prep = NULL) identical(.ld_semver_cmp(ctx, clause), 1L)

.ld_ops <- list(
  `in` = .ld_op_in,
  endsWith = .ld_op_ends_with,
  startsWith = .ld_op_starts_with,
  matches = .ld_op_matches,
  contains = .ld_op_contains,
  lessThan = .ld_op_lt,
  lessThanOrEqual = .ld_op_lte,
  greaterThan = .ld_op_gt,
  greaterThanOrEqual = .ld_op_gte,
  before = .ld_op_before,
  after = .ld_op_after,
  semVerEqual = .ld_op_sem_eq,
  semVerLessThan = .ld_op_sem_lt,
  semVerGreaterThan = .ld_op_sem_gt
)

.ld_match_op <- function(op, ctx_value, clause_value) {
  fn <- .ld_ops[[op]]
  if (is.null(fn)) return(FALSE)
  isTRUE(fn(ctx_value, clause_value, NULL))
}
