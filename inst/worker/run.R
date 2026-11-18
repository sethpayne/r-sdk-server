# Worker entry — FDv2 polling (M4); streaming in M7. NDJSON on stdout; commands on stdin.

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
running <- TRUE
status <- "initializing"
selector <- list(state = "", version = 0L)
poll_interval <- 30
last_poll_at <- -Inf
started <- FALSE

handle_start <- function(payload) {
  config <<- payload
  poll_interval <<- as.numeric(config$poll_interval %||% 30)
  started <<- TRUE
  if (isTRUE(config$offline)) {
    store$mark_initialized(TRUE)
    status <<- "off"
    emit("Ready", list(offline = TRUE))
    emit("Status", list(state = status))
    return(invisible(NULL))
  }

  result <- fdv2_poll_once(config, NULL)
  last_poll_at <<- proc.time()[["elapsed"]]
  if (isTRUE(result$fallback_to_fdv1)) {
    emit_log("warn", "FDv1 fallback requested by server; unsupported — fail-safe", "fdv1Fallback")
    status <<- "off"
    emit("Status", list(state = status, error = "fdv1Fallback"))
    emit("Ready", list(fail_safe = TRUE))
    return(invisible(NULL))
  }
  if (!isTRUE(result$ok)) {
    emit_log("warn", paste("Initial poll failed:", result$error %||% "unknown"))
    status <<- "interrupted"
    emit("Status", list(state = status, error = result$error))
    emit("Ready", list(initialized = FALSE))
  } else {
    store$apply_changeset(result$changeset)
    selector <<- store$selector()
    status <<- "valid"
    emit("StoreSnapshot", store$to_snapshot())
    emit("Status", list(state = status))
    emit("Ready", list(initialized = TRUE))
  }
  invisible(NULL)
}

poll_loop_once <- function() {
  if (is.null(config) || isTRUE(config$offline) || identical(status, "off")) return()
  result <- fdv2_poll_once(config, selector)
  last_poll_at <<- proc.time()[["elapsed"]]
  if (isTRUE(result$fallback_to_fdv1)) {
    emit_log("warn", "FDv1 fallback requested; fail-safe", "fdv1Fallback")
    status <<- "off"
    emit("Status", list(state = status, error = "fdv1Fallback"))
    return()
  }
  if (!isTRUE(result$ok)) {
    status <<- "interrupted"
    emit("Status", list(state = status, error = result$error))
    return()
  }
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

stdin_con <- file("stdin", open = "r", blocking = FALSE)

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
      running <- FALSE
      emit("EventsAck", list(shutdown = TRUE))
      break
    } else if (identical(type, "Ping")) {
      emit("Pong", list(), id = msg$id)
    } else if (identical(type, "Flush")) {
      emit("EventsAck", list(flushed = TRUE))
    }
  }

  if (!isTRUE(running)) break

  now <- proc.time()[["elapsed"]]
  if (isTRUE(started) && !isTRUE(config$offline) && !identical(status, "off") &&
      (now - last_poll_at) >= poll_interval) {
    poll_loop_once()
  }

  Sys.sleep(0.05)
}

quit(save = "no", status = 0)
