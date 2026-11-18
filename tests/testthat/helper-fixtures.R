# FDv2 payload builders and JSON fixture loading.

fixture_path <- function(...) testthat::test_path("fixtures", ...)

# Parse JSON exactly as the worker does (integers stay integers).
from_json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)

read_fixture <- function(...) from_json(paste(readLines(fixture_path(...), warn = FALSE), collapse = "\n"))

# On flag serving variation 1 (`value_on`) on fallthrough; `...` overrides or
# adds fields.
simple_flag <- function(key, value_on = TRUE, version = 1L, ...,
                        value_off = if (is.logical(value_on)) !value_on else "off") {
  flag <- list(
    key = key, version = version, on = TRUE, salt = "salt",
    variations = list(value_off, value_on), offVariation = 0L,
    fallthrough = list(variation = 1L)
  )
  extra <- list(...)
  flag[names(extra)] <- extra
  flag
}

fdv2_events <- function(flags = list(), segments = list(), state = "state-1", version = 1L,
                        intent = "xfer-full") {
  ev <- list(list(event = "server-intent", data = list(
    payloads = list(list(intentCode = intent, id = "p", target = version, reason = "test"))
  )))
  for (f in flags) {
    ev[[length(ev) + 1L]] <- list(event = "put-object", data = list(
      kind = "flag", key = f$key, version = f$version %||% 1L, object = f
    ))
  }
  for (s in segments) {
    ev[[length(ev) + 1L]] <- list(event = "put-object", data = list(
      kind = "segment", key = s$key, version = s$version %||% 1L, object = s
    ))
  }
  ev[[length(ev) + 1L]] <- list(event = "payload-transferred", data = list(state = state, version = version))
  ev
}

fdv2_poll_body <- function(flags = list(), segments = list(), state = "state-1", version = 1L) {
  as.character(jsonlite::toJSON(
    list(events = fdv2_events(flags, segments, state, version)),
    auto_unbox = TRUE, null = "null", digits = NA
  ))
}

fdv2_sse_body <- function(flags = list(), segments = list(), state = "state-1", version = 1L) {
  ev <- fdv2_events(flags, segments, state, version)
  paste0(vapply(ev, function(e) {
    paste0("event: ", e$event, "\ndata: ",
           jsonlite::toJSON(e$data, auto_unbox = TRUE, null = "null", digits = NA), "\n\n")
  }, character(1)), collapse = "")
}

poll_ok <- function(flags = list(), ...) {
  list(status = 200L, headers = list(`Content-Type` = "application/json"),
       body = fdv2_poll_body(flags, ...))
}
