#' LaunchDarkly server-side client
#'
#' @description Evaluations read a parent-side store snapshot updated by the
#' callr worker (see DESIGN.md). Offline mode skips the worker.
#'
#' @export
LdClient <- R6::R6Class(
  "LdClient",
  public = list(
    #' @param config An [LdConfig] or list from [ld_config()].
    #' @param start_wait Seconds to wait for Ready (default from config / 5).
    initialize = function(config, start_wait = NULL) {
      if (!inherits(config, "LdConfig")) {
        stop("config must be an LdConfig from ld_config()", call. = FALSE)
      }
      private$config_ <- config
      private$store_ <- FeatureStore$new()
      private$owner_pid_ <- Sys.getpid()
      private$closed_ <- FALSE
      private$initialized_ <- FALSE
      private$fork_warned_ <- FALSE
      private$data_status_ <- if (isTRUE(config$offline)) "off" else "initializing"
      private$hooks_ <- list()
      private$flag_tracker_ <- FlagTracker$new()

      wait <- start_wait %||% config$start_wait %||% 5

      if (isTRUE(config$offline)) {
        private$store_$mark_initialized(TRUE)
        private$initialized_ <- TRUE
        private$data_status_ <- "off"
        return(invisible(self))
      }

      private$worker_ <- ProcessWorker$new(on_message = function(msg) {
        private$handle_worker_message(msg)
      })
      private$worker_$start()
      private$worker_$send("Start", config$to_list())

      ready <- private$worker_$wait_for_type("Ready", timeout_s = wait)
      # Drain any messages received alongside Ready
      private$worker_$drain()
      if (is.null(ready)) {
        # Timed out — client usable with defaults until data arrives
        private$initialized_ <- private$store_$is_initialized()
      }
      invisible(self)
    },

    is_initialized = function() {
      private$check_fork()
      private$drain()
      isTRUE(private$initialized_)
    },

    is_offline = function() isTRUE(private$config_$offline),

    variation = function(key, context, default) {
      private$check_fork()
      private$drain()
      detail <- private$evaluate_internal(key, context, default)
      detail$value
    },

    variation_detail = function(key, context, default) {
      private$check_fork()
      private$drain()
      private$evaluate_internal(key, context, default)
    },

    all_flags_state = function(context, client_side_only = FALSE, with_reasons = FALSE,
                               details_only_for_tracked_flags = FALSE) {
      private$check_fork()
      private$drain()
      if (!context_valid(context)) {
        return(FeatureFlagsState$new(FALSE))
      }
      state <- FeatureFlagsState$new(private$store_$is_initialized())
      flags <- private$store_$all_flags()
      for (key in names(flags)) {
        flag <- flags[[key]]
        if (isTRUE(flag$deleted)) next
        if (isTRUE(client_side_only) && !isTRUE(flag$clientSide)) next
        detail <- private$evaluate_flag(flag, context, NULL)
        state$add_flag(
          key,
          detail$value,
          variation = detail$variation_index,
          version = flag$version,
          reason = if (isTRUE(with_reasons)) detail$reason else NULL,
          track_events = isTRUE(flag$trackEvents)
        )
      }
      state
    },

    track = function(event_name, context, data = NULL, metric_value = NULL) {
      private$check_fork()
      if (!is.null(private$worker_) && private$worker_$is_alive()) {
        private$worker_$send("Track", list(
          event_name = event_name,
          context = context_to_list(context),
          data = data,
          metric_value = metric_value
        ))
      }
      invisible(NULL)
    },

    identify = function(context) {
      private$check_fork()
      if (!is.null(private$worker_) && private$worker_$is_alive()) {
        private$worker_$send("Identify", list(context = context_to_list(context)))
      }
      invisible(NULL)
    },

    flush = function() {
      private$check_fork()
      if (!is.null(private$worker_) && private$worker_$is_alive()) {
        private$worker_$send("Flush")
      }
      invisible(NULL)
    },

    close = function() {
      if (isTRUE(private$closed_)) return(invisible(NULL))
      private$closed_ <- TRUE
      if (!is.null(private$worker_)) {
        private$worker_$shutdown()
        private$worker_ <- NULL
      }
      invisible(NULL)
    },

    secure_mode_hash = function(context) {
      private$check_fork()
      if (!context_valid(context)) return("")
      key <- context_fully_qualified_key(context)
      digest::hmac(private$config_$sdk_key, key, algo = "sha256", serialize = FALSE)
    },

    add_hook = function(hook) {
      private$hooks_ <- c(private$hooks_, list(hook))
      invisible(self)
    },

    flag_tracker = function() private$flag_tracker_,

    postfork = function() {
      # Tear down inherited worker handles and start fresh in this PID
      private$owner_pid_ <- Sys.getpid()
      private$fork_warned_ <- FALSE
      if (!is.null(private$worker_)) {
        try(private$worker_$shutdown(timeout_s = 0.5), silent = TRUE)
        private$worker_ <- NULL
      }
      private$store_ <- FeatureStore$new()
      private$initialized_ <- FALSE
      if (isTRUE(private$config_$offline)) {
        private$store_$mark_initialized(TRUE)
        private$initialized_ <- TRUE
        return(invisible(self))
      }
      private$worker_ <- ProcessWorker$new(on_message = function(msg) {
        private$handle_worker_message(msg)
      })
      private$worker_$start()
      private$worker_$send("Start", private$config_$to_list())
      private$worker_$wait_for_type("Ready", timeout_s = private$config_$start_wait %||% 5)
      private$worker_$drain()
      invisible(self)
    },

    #' Inject store data (tests / test data source)
    #' @keywords internal
    .update_store_for_tests = function(flags = list(), segments = list()) {
      private$store_$replace_all(flags, segments)
      private$initialized_ <- TRUE
      invisible(self)
    }
  ),
  private = list(
    config_ = NULL,
    store_ = NULL,
    worker_ = NULL,
    owner_pid_ = NULL,
    closed_ = FALSE,
    initialized_ = FALSE,
    fork_warned_ = FALSE,
    data_status_ = NULL,
    hooks_ = NULL,
    flag_tracker_ = NULL,

    check_fork = function() {
      if (Sys.getpid() != private$owner_pid_) {
        if (!isTRUE(private$fork_warned_)) {
          warning(
            "LdClient used after fork without postfork(); returning defaults until re-initialized",
            call. = FALSE
          )
          private$fork_warned_ <- TRUE
        }
      }
    },

    drain = function() {
      if (!is.null(private$worker_) && private$worker_$is_alive()) {
        private$worker_$drain()
      }
    },

    handle_worker_message = function(msg) {
      type <- msg$type
      payload <- msg$payload %||% list()
      if (identical(type, "StoreSnapshot")) {
        old <- private$store_$all_flags()
        private$store_$replace_all(
          flags = payload$flags %||% list(),
          segments = payload$segments %||% list(),
          selector = payload$selector
        )
        private$initialized_ <- TRUE
        private$flag_tracker_$notify_store_update(
          old,
          private$store_$all_flags(),
          eval_fn = function(key, ctx) private$evaluate_internal(key, ctx, NULL)$value
        )
      } else if (identical(type, "Ready")) {
        if (isTRUE(payload$initialized) || isTRUE(payload$offline) || isTRUE(payload$fail_safe)) {
          private$initialized_ <- TRUE
        }
      } else if (identical(type, "Status")) {
        private$data_status_ <- payload$state %||% private$data_status_
      } else if (identical(type, "Log")) {
        # Never log payload that might contain keys; worker already redacts
        message(sprintf("[ld:%s] %s", payload$level %||% "info", payload$message %||% ""))
      } else if (identical(type, "Fatal")) {
        warning(payload$message %||% "worker fatal", call. = FALSE)
      }
    },

    evaluate_internal = function(key, context, default) {
      if (Sys.getpid() != private$owner_pid_) {
        return(evaluation_detail(default, NULL, list(kind = "ERROR", errorKind = "CLIENT_NOT_READY")))
      }
      if (!context_valid(context)) {
        return(evaluation_detail(default, NULL, list(kind = "ERROR", errorKind = "USER_NOT_SPECIFIED")))
      }
      flag <- private$store_$get_flag(key)
      if (is.null(flag) || isTRUE(flag$deleted)) {
        return(evaluation_detail(default, NULL, list(kind = "ERROR", errorKind = "FLAG_NOT_FOUND")))
      }
      private$evaluate_flag(flag, context, default)
    },

    evaluate_flag = function(flag, context, default) {
      eval_fn <- function(series_ctx) {
        ld_evaluate_flag(
          flag,
          series_ctx$context %||% context,
          default,
          get_flag = function(k) private$store_$get_flag(k),
          get_segment = function(k) private$store_$get_segment(k)
        )
      }
      if (length(private$hooks_) == 0L) {
        return(eval_fn(list(context = context, flag_key = flag$key)))
      }
      .ld_with_hooks(private$hooks_, list(context = context, flag_key = flag$key), eval_fn)
    }
  )
)

#' Construct an [LdClient]
#' @param config [LdConfig]
#' @param start_wait Optional override for start wait seconds.
#' @export
ld_client <- function(config, start_wait = NULL) {
  LdClient$new(config, start_wait = start_wait)
}
