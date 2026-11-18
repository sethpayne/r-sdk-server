#' Clause operators (parity with python-server-sdk operators.py)
#'
#' @keywords internal
NULL

.ld_is_number <- function(x) {
  is.numeric(x) && !is.logical(x) && length(x) == 1L && !is.na(x)
}

.ld_op_in <- function(ctx, clause, prep = NULL) {
  if (is.logical(ctx) != is.logical(clause)) return(FALSE)
  identical(ctx, clause)
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

# Time ops: accept epoch millis (numeric) or ISO-8601 strings
.ld_parse_time_ms <- function(x) {
  if (.ld_is_number(x)) return(as.numeric(x))
  if (!is.character(x)) return(NULL)
  t <- suppressWarnings(as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"))
  if (is.na(t)) t <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (is.na(t)) return(NULL)
  as.numeric(t) * 1000
}

.ld_op_before <- function(ctx, clause, prep = NULL) {
  a <- .ld_parse_time_ms(ctx); b <- .ld_parse_time_ms(clause)
  !is.null(a) && !is.null(b) && a < b
}
.ld_op_after <- function(ctx, clause, prep = NULL) {
  a <- .ld_parse_time_ms(ctx); b <- .ld_parse_time_ms(clause)
  !is.null(a) && !is.null(b) && a > b
}

# Semver: lightweight numeric tuple compare (major.minor.patch); no prerelease
.ld_parse_semver <- function(x) {
  if (!is.character(x) || length(x) != 1L) return(NULL)
  m <- regmatches(x, regexec("^([0-9]+)(?:\\.([0-9]+))?(?:\\.([0-9]+))?", x))[[1]]
  if (length(m) < 2L) return(NULL)
  as.integer(c(m[2], if (nzchar(m[3] %||% "")) m[3] else "0", if (nzchar(m[4] %||% "")) m[4] else "0"))
}

.ld_semver_cmp <- function(a, b) {
  aa <- .ld_parse_semver(a); bb <- .ld_parse_semver(b)
  if (is.null(aa) || is.null(bb)) return(NA_integer_)
  for (i in 1:3) {
    if (aa[i] < bb[i]) return(-1L)
    if (aa[i] > bb[i]) return(1L)
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
