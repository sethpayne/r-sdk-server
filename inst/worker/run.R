# Worker — FDv2 streaming (preferred) with polling fallback; events buffer (M6–M7).

`%||%` <- function(a, b) if (is.null(a)) b else a

emit <- function(type, payload = list(), id = NULL) {
  msg <- list(v = 1L, type = type, payload = payload)
  if (!is.null(id)) msg$id <- id
  cat(jsonlite::toJSON(msg, auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
  flush(stdout())
}

emit_log <- function(level, message, code = NULL) {
  emit("Log", list(level = level, message = message, code = code))
}

lib <- Sys.getenv("LAUNCHDARKLY_WORKER_LIB", unset = "")
if (nzchar(lib)) .libPaths(c(strsplit(lib, .Platform$path.sep, fixed = TRUE)[[1]], .libPaths()))
pkg_path <- Sys.getenv("LAUNCHDARKLY_PKG_PATH", unset = "")
loaded <- FALSE
# Dev: load_all only for a SOURCE tree (R/*.R files). An installed package also
# has DESCRIPTION + R/, but load_all() there yields an empty namespace.
r_src <- if (nzchar(pkg_path)) {
  list.files(file.path(pkg_path, "R"), pattern = "\\.[Rr]$", full.names = FALSE)
} else {
  character()
}
if (nzchar(pkg_path) &&
    file.exists(file.path(pkg_path, "DESCRIPTION")) &&
    length(r_src) > 0L &&
    requireNamespace("pkgload", quietly = TRUE)) {
  loaded <- isTRUE(tryCatch({
    pkgload::load_all(pkg_path, quiet = TRUE)
    TRUE
  }, error = function(e) FALSE))
}
if (!loaded && requireNamespace("launchdarklyServer", quietly = TRUE)) {
  library(launchdarklyServer)
  loaded <- TRUE
}
if (!loaded) {
  emit("Fatal", list(message = "launchdarklyServer not available in worker"))
  quit(save = "no", status = 1)
}

# library() only attaches exports; bind internals the worker needs.
.ld_ns <- tryCatch(asNamespace("launchdarklyServer"), error = function(e) NULL)
if (!is.null(.ld_ns)) {
  for (sym in c("fdv2_poll_once", "fdv2_stream_once", "ld_post_events")) {
    if (!exists(sym, inherits = FALSE) && exists(sym, envir = .ld_ns, inherits = FALSE)) {
      assign(sym, get(sym, envir = .ld_ns), envir = environment())
    }
  }
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
events_disabled <- FALSE
last_context_keys_reset <- proc.time()[["elapsed"]]

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

do_poll <- function(use_sync_uri = FALSE) {
  uri <- NULL
  if (isTRUE(use_sync_uri) && !is.null(config$sync_poll_uri) && nzchar(config$sync_poll_uri)) {
    uri <- config$sync_poll_uri
  }
  result <- fdv2_poll_once(config, selector, base_uri = uri)
  last_sync_at <<- proc.time()[["elapsed"]]
  if (isTRUE(result$fallback_to_fdv1)) {
    handle_fdv1_fallback()
    emit("Ready", list(fail_safe = TRUE))
    return(result)
  }
  if (!isTRUE(result$ok)) {
    status <<- "interrupted"
    emit("Status", list(state = status, error = result$error))
    # Optional secondary poll fallback URI
    if (!is.null(config$fallback_poll_uri) && nzchar(config$fallback_poll_uri)) {
      result2 <- fdv2_poll_once(config, selector, base_uri = config$fallback_poll_uri)
      if (isTRUE(result2$ok)) {
        apply_ok_changeset(result2)
        return(result2)
      }
    }
    # 401/403: the SDK key is rejected; retrying cannot succeed.
    if (!is.null(result$status) && result$status %in% c(401L, 403L)) {
      emit_log("error", sprintf(
        "Polling received HTTP %s; SDK key rejected, stopping data source", result$status
      ), "unrecoverable")
      status <<- "off"
      emit("Status", list(state = status, error = result$error))
    }
    return(result)
  }
  apply_ok_changeset(result)
  result
}

handle_message <- function(msg) {
  type <- msg$type
  if (identical(type, "Start")) {
    started_ok <- tryCatch({
      handle_start(msg$payload)
      TRUE
    }, error = function(e) {
      emit_log("error", paste("Worker start failed:", conditionMessage(e)), "startError")
      FALSE
    })
    if (!isTRUE(started_ok)) {
      status <<- "interrupted"
      emit("Status", list(state = status, error = "start failed"))
      emit("Ready", list(initialized = isTRUE(store$is_initialized())))
      last_sync_at <<- -Inf
    }
  } else if (identical(type, "Shutdown")) {
    flush_events()
    running <<- FALSE
  } else if (identical(type, "Ping")) {
    emit("Pong", list(), id = msg$id)
  } else if (identical(type, "Flush")) {
    flush_events()
  } else if (identical(type, "Track")) {
    if (!is.null(buffer) && !events_disabled) {
      p <- msg$payload
      buffer$process_custom(p$event_name, p$context, p$data, p$metric_value,
                            creation_date = p$creation_date)
    }
  } else if (identical(type, "Identify")) {
    if (!is.null(buffer) && !events_disabled) {
      buffer$process_identify(msg$payload$context, creation_date = msg$payload$creation_date)
    }
  } else if (identical(type, "Events")) {
    if (!is.null(buffer) && !events_disabled) {
      for (rec in msg$payload$events %||% list()) buffer$process_eval(rec)
    }
  }
  invisible(NULL)
}

read_ipc <- function() {
  repeat {
    line <- tryCatch(readLines(stdin_con, n = 1L, warn = FALSE), error = function(e) character())
    if (length(line) == 0L) break
    msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(msg) || !is.list(msg) || is.null(names(msg)) || is.null(msg$type)) next
    tryCatch(handle_message(msg), error = function(e) {
      emit_log("error", paste("Worker failed to handle", msg$type, "message:", conditionMessage(e)))
    })
    if (!isTRUE(running)) break
  }
  invisible(NULL)
}

pump_ipc <- function() {
  # Drain stdin while a long-lived stream is open so Flush/Track/Shutdown still work.
  read_ipc()
}

do_stream <- function(until_first_payload = TRUE) {
  # During init, stop at first payload so Ready can be emitted. The main loop
  # may keep the connection open (until_first_payload = FALSE) for multi-update.
  on_cs <- function(cs) {
    apply_ok_changeset(list(ok = TRUE, changeset = cs))
    selector <<- store$selector()
  }
  result <- fdv2_stream_once(
    config,
    selector,
    on_changeset = on_cs,
    should_stop = function() {
      pump_ipc()
      !isTRUE(running)
    },
    until_first_payload = until_first_payload
  )
  last_sync_at <<- proc.time()[["elapsed"]]
  if (isTRUE(result$fallback_to_fdv1)) {
    handle_fdv1_fallback()
    return(result)
  }
  # Permanent primary removal (401/403): switch to fallback stream if configured
  status_code <- result$status
  if (is.null(status_code) && grepl("HTTP (401|403)", result$error %||% "")) {
    status_code <- as.integer(sub(".*HTTP ([0-9]+).*", "\\1", result$error))
  }
  if (!isTRUE(result$ok) && !is.null(status_code) && status_code %in% c(401L, 403L)) {
    fb <- config$fallback_stream_uri
    emit_log("info", sprintf(
      "Primary stream status=%s fallback=%s err=%s",
      status_code, fb %||% "NULL", result$error %||% ""
    ))
    if (!is.null(fb) && nzchar(fb)) {
      emit_log("info", "Primary stream permanently failed; switching to secondary")
      # Must <<- — `config$x <-` inside a function only mutates a local copy.
      config$stream_uri <<- fb
      config$fallback_stream_uri <<- NULL
      result <- fdv2_stream_once(
        config, selector,
        on_changeset = on_cs,
        until_first_payload = until_first_payload
      )
      last_sync_at <<- proc.time()[["elapsed"]]
      if (isTRUE(result$ok)) return(result)
      emit_log("warn", sprintf("Secondary stream failed: %s", result$error %||% "unknown"))
    }
  }
  if (!isTRUE(result$ok)) {
    # goodbye / malformed → reconnect; don't mark permanently off
    status <<- "interrupted"
    emit("Status", list(state = status, error = result$error))
    return(result)
  }
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
    private_attrs = unlist(config$private_attributes %||% list(), use.names = FALSE),
    context_keys_capacity = config$context_keys_capacity %||% 1000L,
    omit_anonymous_contexts = isTRUE(config$omit_anonymous_contexts)
  )
  started <<- TRUE

  if (isTRUE(config$offline)) {
    store$mark_initialized(TRUE)
    status <<- "off"
    emit("Ready", list(offline = TRUE))
    emit("Status", list(state = status))
    return(invisible(NULL))
  }

  # Initializer path: poll every configured polling initializer in order, then
  # (optionally) stream/poll the primary synchronizer once so Ready can fire.
  result <- list(ok = FALSE)
  init_uris <- unlist(config$poll_init_uris %||% list(), use.names = FALSE)
  if (length(init_uris) == 0L &&
      !isTRUE(config$stream_only) &&
      !is.null(config$base_uri) && nzchar(config$base_uri)) {
    init_uris <- config$base_uri
  }

  if (isTRUE(config$stream_only) || (isTRUE(use_stream) && isTRUE(config$skip_poll_init))) {
    result <- do_stream(until_first_payload = TRUE)
  } else {
    for (uri in init_uris) {
      result <- fdv2_poll_once(config, selector, base_uri = uri)
      last_sync_at <<- proc.time()[["elapsed"]]
      if (isTRUE(result$fallback_to_fdv1)) {
        handle_fdv1_fallback()
        emit("Ready", list(fail_safe = TRUE))
        return(invisible(NULL))
      }
      if (isTRUE(result$ok)) apply_ok_changeset(result)
    }
    # After initializers, contact the synchronizer once before Ready so harness
    # suites that assert on the sync endpoint immediately after construction pass.
    if (isTRUE(use_stream)) {
      result <- do_stream(until_first_payload = TRUE)
    } else if (!is.null(config$sync_poll_uri) && nzchar(config$sync_poll_uri)) {
      result <- do_poll(use_sync_uri = TRUE)
    } else if (length(init_uris) == 0L) {
      result <- do_poll(use_sync_uri = FALSE)
    }
  }
  if (identical(status, "off")) {
    emit("Ready", list(fail_safe = TRUE))
    return(invisible(NULL))
  }
  # Allow the main loop to reconnect/poll immediately after Ready.
  last_sync_at <<- -Inf
  initialized <- isTRUE(result$ok) || isTRUE(store$is_initialized()) ||
    identical(status, "valid")
  emit("Ready", list(initialized = initialized))
  invisible(NULL)
}

flush_events <- function() {
  if (is.null(buffer) || !isTRUE(config$send_events) || events_disabled) {
    emit("EventsAck", list(flushed = TRUE, count = 0))
    return()
  }
  ev <- buffer$payload()
  res <- ld_post_events(config, ev)
  if (isTRUE(res$unrecoverable)) {
    events_disabled <<- TRUE
    emit_log("error", sprintf("Event delivery received %s; analytics events disabled",
                              res$error %||% "an unrecoverable error"), "eventsDisabled")
  }
  emit("EventsAck", list(flushed = isTRUE(res$ok), count = length(ev), error = res$error))
}

stdin_con <- file("stdin", open = "r", blocking = FALSE)
last_flush_at <- proc.time()[["elapsed"]]

worker_step <- function() {
  read_ipc()
  if (!isTRUE(running)) return(invisible(NULL))

  now <- proc.time()[["elapsed"]]
  if (isTRUE(started) && !isTRUE(config$offline) && !identical(status, "off")) {
    if (identical(sync_mode, "stream")) {
      # Attempt stream; on failure fall back to polling after thresholds.
      # Keep the connection open for multi-payload FDv2 suites.
      if ((now - last_sync_at) >= reconnect_delay) {
        res <- do_stream(until_first_payload = FALSE)
        if (!isTRUE(res$ok) && !identical(status, "off")) {
          # Fallback to polling synchronizer
          sync_mode <<- "poll"
          emit_log("info", "Streaming interrupted; falling back to polling")
        }
      }
    } else if ((now - last_sync_at) >= poll_interval) {
      res <- do_poll(use_sync_uri = TRUE)
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
    last_flush_at <<- now
    flush_events()
  }
  keys_iv <- as.numeric(config$context_keys_flush_interval %||% 300)
  if (!is.null(buffer) && (now - last_context_keys_reset) >= keys_iv) {
    buffer$reset_context_keys()
    last_context_keys_reset <<- now
  }
  invisible(NULL)
}

# A failure in one iteration (unexpected payload shape, transient I/O error)
# must not end the worker: log it, mark the data source interrupted, back off.
while (isTRUE(running)) {
  tryCatch(worker_step(), error = function(e) {
    emit_log("error", paste("Worker iteration failed:", conditionMessage(e)), "workerError")
    if (!identical(status, "off")) {
      status <<- "interrupted"
      emit("Status", list(state = status, error = conditionMessage(e)))
    }
    last_sync_at <<- proc.time()[["elapsed"]]
  })
  if (!isTRUE(running)) break
  Sys.sleep(0.05)
}

quit(save = "no", status = 0)
