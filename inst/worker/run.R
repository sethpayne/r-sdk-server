# Worker — FDv2 streaming (preferred) with polling fallback; events buffer (M6–M7).

`%||%` <- function(a, b) if (is.null(a)) b else a

emit <- function(type, payload = list(), id = NULL) {
  msg <- list(v = 1L, type = type, payload = payload)
  if (!is.null(id)) msg$id <- id
  cat(jsonlite::toJSON(msg, auto_unbox = TRUE, null = "null"), "\n", sep = "")
  flush(stdout())
}

emit_log <- function(level, message, code = NULL) {
  emit("Log", list(level = level, message = message, code = code))
}

lib <- Sys.getenv("LAUNCHDARKLY_WORKER_LIB", unset = "")
if (nzchar(lib)) .libPaths(c(strsplit(lib, .Platform$path.sep, fixed = TRUE)[[1]], .libPaths()))
pkg_path <- Sys.getenv("LAUNCHDARKLY_PKG_PATH", unset = "")
if (nzchar(pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_path, quiet = TRUE)
} else if (requireNamespace("launchdarklyServer", quietly = TRUE)) {
  library(launchdarklyServer)
} else {
  emit("Fatal", list(message = "launchdarklyServer not available in worker"))
  quit(save = "no", status = 1)
}

config <- NULL
store <- FeatureStore$new()
buffer <- NULL
running <- TRUE
status <- "initializing"
selector <- list(state = "", version = 0L)
poll_interval <- 30
reconnect_delay <- 1
last_sync_at <- -Inf
started <- FALSE
use_stream <- TRUE
seconds_valid <- 0
sync_mode <- "stream" # stream | poll

handle_fdv1_fallback <- function() {
  emit_log("warn", "FDv1 fallback requested by server; unsupported — fail-safe", "fdv1Fallback")
  status <<- "off"
  emit("Status", list(state = status, error = "fdv1Fallback"))
}

apply_ok_changeset <- function(result) {
  if (isTRUE(result$changeset$no_changes)) {
    status <<- "valid"
    emit("Status", list(state = status))
    return()
  }
  store$apply_changeset(result$changeset)
  selector <<- store$selector()
  status <<- "valid"
  emit("StoreSnapshot", store$to_snapshot())
  emit("Status", list(state = status))
}

do_poll <- function() {
  result <- fdv2_poll_once(config, selector)
  last_sync_at <<- proc.time()[["elapsed"]]
  if (isTRUE(result$fallback_to_fdv1)) {
    handle_fdv1_fallback()
    emit("Ready", list(fail_safe = TRUE))
    return(result)
  }
  if (!isTRUE(result$ok)) {
    status <<- "interrupted"
    emit("Status", list(state = status, error = result$error))
    return(result)
  }
  apply_ok_changeset(result)
  result
}

do_stream <- function() {
  result <- fdv2_stream_once(config, selector)
  last_sync_at <<- proc.time()[["elapsed"]]
  if (isTRUE(result$fallback_to_fdv1)) {
    handle_fdv1_fallback()
    return(result)
  }
  if (!isTRUE(result$ok)) {
    status <<- "interrupted"
    emit("Status", list(state = status, error = result$error))
    return(result)
  }
  apply_ok_changeset(result)
  result
}

handle_start <- function(payload) {
  config <<- payload
  poll_interval <<- as.numeric(config$poll_interval %||% 30)
  reconnect_delay <<- as.numeric(config$initial_reconnect_delay %||% 1)
  use_stream <<- isTRUE(config$stream)
  sync_mode <<- if (use_stream) "stream" else "poll"
  buffer <<- EventBuffer$new(
    capacity = config$events_capacity %||% 10000L,
    all_private = isTRUE(config$all_attributes_private),
    private_attrs = unlist(config$private_attributes %||% list(), use.names = FALSE)
  )
  started <<- TRUE

  if (isTRUE(config$offline)) {
    store$mark_initialized(TRUE)
    status <<- "off"
    emit("Ready", list(offline = TRUE))
    emit("Status", list(state = status))
    return(invisible(NULL))
  }

  # Initializer: always poll once for basis (parity with default chain)
  result <- do_poll()
  if (identical(status, "off")) {
    emit("Ready", list(fail_safe = TRUE))
    return(invisible(NULL))
  }
  if (!isTRUE(result$ok)) {
    emit("Ready", list(initialized = FALSE))
  } else {
    emit("Ready", list(initialized = TRUE))
  }
  invisible(NULL)
}

flush_events <- function() {
  if (is.null(buffer) || !isTRUE(config$send_events)) {
    emit("EventsAck", list(flushed = TRUE, count = 0))
    return()
  }
  ev <- buffer$drain()
  res <- ld_post_events(config, ev)
  emit("EventsAck", list(flushed = isTRUE(res$ok), count = length(ev), error = res$error))
}

stdin_con <- file("stdin", open = "r", blocking = FALSE)
last_flush_at <- proc.time()[["elapsed"]]

while (isTRUE(running)) {
  repeat {
    line <- tryCatch(readLines(stdin_con, n = 1L, warn = FALSE), error = function(e) character())
    if (length(line) == 0L) break
    msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(msg) || is.null(msg$type)) next
    type <- msg$type
    if (identical(type, "Start")) {
      handle_start(msg$payload)
    } else if (identical(type, "Shutdown")) {
      flush_events()
      running <- FALSE
      break
    } else if (identical(type, "Ping")) {
      emit("Pong", list(), id = msg$id)
    } else if (identical(type, "Flush")) {
      flush_events()
    } else if (identical(type, "Track")) {
      if (!is.null(buffer)) {
        buffer$add_custom(msg$payload$event_name, msg$payload$context, msg$payload$data, msg$payload$metric_value)
      }
    } else if (identical(type, "Identify")) {
      if (!is.null(buffer)) buffer$add_identify(msg$payload$context)
    }
  }
  if (!isTRUE(running)) break

  now <- proc.time()[["elapsed"]]
  if (isTRUE(started) && !isTRUE(config$offline) && !identical(status, "off")) {
    if (identical(sync_mode, "stream")) {
      # Attempt stream; on failure fall back to polling after thresholds
      if ((now - last_sync_at) >= reconnect_delay) {
        res <- do_stream()
        if (!isTRUE(res$ok) && !identical(status, "off")) {
          # Fallback to polling synchronizer
          sync_mode <<- "poll"
          emit_log("info", "Streaming interrupted; falling back to polling")
        }
      }
    } else if ((now - last_sync_at) >= poll_interval) {
      res <- do_poll()
      # Recovery: after sustained valid polling, try stream again
      if (isTRUE(res$ok) && isTRUE(use_stream)) {
        seconds_valid <<- seconds_valid + poll_interval
        if (seconds_valid >= 60) {
          sync_mode <<- "stream"
          seconds_valid <<- 0
          emit_log("info", "Recovering to streaming synchronizer")
        }
      } else {
        seconds_valid <<- 0
      }
    }
  }

  flush_iv <- as.numeric(config$flush_interval %||% 5)
  if (isTRUE(started) && isTRUE(config$send_events) && (now - last_flush_at) >= flush_iv) {
    flush_events()
    last_flush_at <- now
  }

  Sys.sleep(0.05)
}

quit(save = "no", status = 0)
