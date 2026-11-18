# Client constructors that always close (and stop their worker) at test exit.

local_ld_client <- function(..., start_wait = NULL, .env = parent.frame()) {
  client <- ld_client(ld_config(...), start_wait = start_wait)
  withr::defer(client$close(), envir = .env)
  client
}

local_offline_client <- function(flags = list(), segments = list(), .env = parent.frame()) {
  client <- local_ld_client("sdk-key", offline = TRUE, .env = .env)
  if (length(flags) || length(segments)) {
    names(flags) <- vapply(flags, function(f) f$key, character(1))
    if (length(segments)) names(segments) <- vapply(segments, function(s) s$key, character(1))
    client$.update_store_for_tests(flags = flags, segments = segments)
  }
  client
}

# Client polling the mock: poll_interval long so tests control timing.
local_polling_client <- function(mock, sdk_key = "sdk-key-123456", ..., .env = parent.frame()) {
  args <- utils::modifyList(list(
    base_uri = mock$url, events_uri = mock$url, stream = FALSE,
    poll_interval = 60, start_wait = 10, flush_interval = 60
  ), list(...))
  client <- ld_client(do.call(ld_config, c(list(sdk_key), args)))
  withr::defer(client$close(), envir = .env)
  client
}

worker_of <- function(client) client$.__enclos_env__$private$worker_

process_alive <- function(pid) {
  !is.na(pid) && isTRUE(tools::pskill(pid, 0L))
}
