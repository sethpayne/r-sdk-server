#' callr / processx worker supervision + NDJSON IPC
#'
#' @keywords internal
NULL

#' @export
ProcessWorker <- R6::R6Class(
  "ProcessWorker",
  public = list(
    initialize = function(on_message = NULL) {
      private$on_message_ <- on_message
      private$started_ <- FALSE
      invisible(self)
    },

    start = function() {
      if (isTRUE(private$started_)) return(invisible(self))
      script <- system.file("worker", "run.R", package = "launchdarklyServer")
      if (!nzchar(script) || !file.exists(script)) {
        # Dev load: find inst/worker relative to package source
        script <- private$dev_worker_script()
      }
      if (is.null(script) || !file.exists(script)) {
        stop("Worker script not found (inst/worker/run.R)", call. = FALSE)
      }

      env <- Sys.getenv()
      env[["LAUNCHDARKLY_WORKER_LIB"]] <- paste(.libPaths(), collapse = .Platform$path.sep)
      env[["LAUNCHDARKLY_PKG_PATH"]] <- private$pkg_path()

      private$proc_ <- processx::process$new(
        "Rscript",
        c(script),
        stdin = "|",
        stdout = "|",
        stderr = "|",
        env = env,
        supervise = TRUE
      )
      private$started_ <- TRUE
      invisible(self)
    },

    is_alive = function() {
      !is.null(private$proc_) && private$proc_$is_alive()
    },

    send = function(type, payload = list(), id = NULL) {
      if (!self$is_alive()) stop("Worker is not running", call. = FALSE)
      msg <- list(v = 1L, type = type, payload = payload)
      if (!is.null(id)) msg$id <- id
      line <- paste0(jsonlite::toJSON(msg, auto_unbox = TRUE, null = "null"), "\n")
      private$proc_$write_input(line)
      invisible(self)
    },

    #' Read and dispatch available stdout lines.
    drain = function(timeout_ms = 0) {
      if (!self$is_alive()) return(invisible(list()))
      out <- private$proc_$read_output_lines()
      msgs <- list()
      for (line in out) {
        if (!nzchar(trimws(line))) next
        msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
        if (is.null(msg)) next
        msgs[[length(msgs) + 1L]] <- msg
        if (is.function(private$on_message_)) private$on_message_(msg)
      }
      # Also drain stderr to logs (never echo secrets)
      err <- private$proc_$read_error_lines()
      if (length(err)) {
        for (e in err) {
          if (nzchar(e)) message("[worker] ", e)
        }
      }
      invisible(msgs)
    },

    wait_for_type = function(type, timeout_s = 5) {
      deadline <- Sys.time() + timeout_s
      while (Sys.time() < deadline) {
        msgs <- self$drain()
        for (m in msgs) {
          if (identical(m$type, type)) return(m)
        }
        if (!self$is_alive()) return(NULL)
        Sys.sleep(0.05)
      }
      NULL
    },

    shutdown = function(timeout_s = 2) {
      if (!self$is_alive()) {
        private$started_ <- FALSE
        return(invisible(NULL))
      }
      try(self$send("Shutdown"), silent = TRUE)
      private$proc_$wait(timeout = as.integer(timeout_s * 1000))
      if (private$proc_$is_alive()) {
        private$proc_$kill(close_connections = TRUE)
      }
      private$started_ <- FALSE
      invisible(NULL)
    }
  ),
  private = list(
    proc_ = NULL,
    on_message_ = NULL,
    started_ = FALSE,
    pkg_path = function() {
      p <- tryCatch(pkgload::pkg_path(), error = function(e) "")
      if (!nzchar(p)) {
        p <- system.file(package = "launchdarklyServer")
      }
      p
    },
    dev_worker_script = function() {
      root <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)
      if (is.null(root)) return(NULL)
      file.path(root, "inst", "worker", "run.R")
    }
  )
)
