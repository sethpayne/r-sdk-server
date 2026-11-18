#' LaunchDarkly server-side client
#'
#' @description Evaluations read a parent-side store snapshot updated by the
#' callr worker (see DESIGN.md). Offline mode skips the worker. Public methods
#' never throw: invalid input yields the default value with an error reason.
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
      private$restart_delay_ <- 1
      private$flag_tracker_ <- FlagTracker$new(eval_fn = function(key, ctx) {
        tryCatch(private$evaluate_internal(key, ctx, NULL)$detail$value, error = function(e) NULL)
      })

      wait <- start_wait %||% config$start_wait %||% 5

      if (isTRUE(config$offline)) {
        private$store_$mark_initialized(TRUE)
        private$initialized_ <- TRUE
        private$data_status_ <- "off"
        return(invisible(self))
      }

      private$start_worker()
      ready <- private$worker_$wait_for_type("Ready", timeout_s = wait)
      # Drain any messages received alongside Ready (StoreSnapshot may trail)
      private$worker_$drain()
      if (is.null(ready)) {
        # Timed out — client usable with defaults until data arrives
        private$initialized_ <- private$store_$is_initialized()
      } else {
        private$initialized_ <- isTRUE(ready$payload$initialized) ||
          private$store_$is_initialized()
        if (isTRUE(ready$payload$offline) || isTRUE(ready$payload$fail_safe)) {
          private$data_status_ <- "off"
        }
      }
      invisible(self)
    },

    is_initialized = function() {
      private$check_fork()
      private$drain()
      isTRUE(private$initialized_)
    },

    is_offline = function() isTRUE(private$config_$offline),

    #' @description Current data source state: `initializing`, `valid`,
    #'   `interrupted`, or `off`.
    data_source_status = function() {
      private$drain()
      private$data_status_
    },

    variation = function(key, context, default) {
      private$variation_impl(key, context, default, with_reasons = FALSE)$value
    },

    variation_detail = function(key, context, default) {
      private$variation_impl(key, context, default, with_reasons = TRUE)
    },

    all_flags_state = function(context, client_side_only = FALSE, with_reasons = FALSE,
                               details_only_for_tracked_flags = FALSE) {
      private$check_fork()
      private$drain()
      tryCatch(
        private$all_flags_state_impl(context, client_side_only, with_reasons,
                                     details_only_for_tracked_flags),
        error = function(e) FeatureFlagsState$new(FALSE)
      )
    },

    track = function(event_name, context, data = NULL, metric_value = NULL) {
      if (private$forked()) return(invisible(NULL))
      if (!private$events_enabled() || !.ld_is_context(context) || !context_valid(context)) {
        return(invisible(NULL))
      }
      if (!is.character(event_name) || length(event_name) != 1L || is.na(event_name)) {
        return(invisible(NULL))
      }
      private$send_safely("Track", list(
        event_name = event_name,
        context = context_to_list(context),
        data = data,
        metric_value = metric_value,
        creation_date = round(as.numeric(Sys.time()) * 1000)
      ))
      invisible(NULL)
    },

    identify = function(context) {
      if (private$forked()) return(invisible(NULL))
      if (!private$events_enabled() || !.ld_is_context(context) || !context_valid(context)) {
        return(invisible(NULL))
      }
      private$send_safely("Identify", list(
        context = context_to_list(context),
        creation_date = round(as.numeric(Sys.time()) * 1000)
      ))
      invisible(NULL)
    },

    flush = function() {
      if (private$forked()) return(invisible(NULL))
      private$send_safely("Flush")
      invisible(NULL)
    },

    close = function() {
      if (isTRUE(private$closed_)) return(invisible(NULL))
      private$closed_ <- TRUE
      if (!is.null(private$worker_)) {
        if (!private$forked()) try(private$worker_$shutdown(), silent = TRUE)
        private$worker_ <- NULL
      }
      invisible(NULL)
    },

    secure_mode_hash = function(context) {
      private$check_fork()
      if (!.ld_is_context(context) || !context_valid(context)) return("")
      key <- context_fully_qualified_key(context)
      digest::hmac(private$config_$sdk_key, key, algo = "sha256", serialize = FALSE)
    },

    add_hook = function(hook) {
      private$hooks_ <- c(private$hooks_, list(hook))
      invisible(self)
    },

    flag_tracker = function() private$flag_tracker_,

    postfork = function() {
      # Tear down inherited worker handles and start fresh in this PID. The
      # inherited processx handle belongs to the parent, so it is dropped, not
      # shut down (that would stop the parent's worker).
      private$owner_pid_ <- Sys.getpid()
      private$fork_warned_ <- FALSE
      # Keep the inherited handle referenced so its finalizer never kills the
      # parent's worker from this process.
      if (!is.null(private$worker_)) private$inherited_ <- c(private$inherited_, list(private$worker_))
      private$worker_ <- NULL
      private$store_ <- FeatureStore$new()
      private$initialized_ <- FALSE
      if (isTRUE(private$config_$offline)) {
        private$store_$mark_initialized(TRUE)
        private$initialized_ <- TRUE
        return(invisible(self))
      }
      private$start_worker()
      private$worker_$wait_for_type("Ready", timeout_s = private$config_$start_wait %||% 5)
      private$worker_$drain()
      invisible(self)
    },

    #' Inject store data (tests / test data source)
    #' @keywords internal
    .update_store_for_tests = function(flags = list(), segments = list()) {
      old <- private$store_$all_flags()
      private$store_$replace_all(flags, segments)
      private$initialized_ <- TRUE
      private$notify_tracker(old)
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
    restart_at_ = NULL,
    restart_delay_ = 1,
    worker_warned_ = FALSE,
    inherited_ = list(),

    forked = function() {
      private$check_fork()
      !identical(Sys.getpid(), private$owner_pid_)
    },

    check_fork = function() {
      if (!identical(Sys.getpid(), private$owner_pid_)) {
        if (!isTRUE(private$fork_warned_)) {
          warning(
            "LdClient used after fork without postfork(); returning defaults until re-initialized",
            call. = FALSE
          )
          private$fork_warned_ <- TRUE
        }
      }
    },

    events_enabled = function() {
      !isTRUE(private$config_$offline) && isTRUE(private$config_$send_events) &&
        !isTRUE(private$closed_)
    },

    start_worker = function() {
      private$worker_ <- ProcessWorker$new(on_message = function(msg) {
        private$handle_worker_message(msg)
      })
      private$worker_$start()
      private$worker_$send("Start", private$config_$to_list())
    },

    # Restart a worker that exited unexpectedly, with exponential backoff
    # (1s doubling to 30s). The last store snapshot keeps serving meanwhile.
    maybe_restart_worker = function() {
      if (isTRUE(private$closed_) || is.null(private$worker_) || private$forked()) return()
      if (private$worker_$is_alive()) return()
      now <- proc.time()[["elapsed"]]
      if (is.null(private$restart_at_)) {
        private$restart_at_ <- now + private$restart_delay_
        private$data_status_ <- "interrupted"
        if (!isTRUE(private$worker_warned_)) {
          warning("LaunchDarkly worker process exited unexpectedly; restarting", call. = FALSE)
          private$worker_warned_ <- TRUE
        }
        return()
      }
      if (now < private$restart_at_) return()
      private$restart_at_ <- NULL
      private$restart_delay_ <- min(private$restart_delay_ * 2, 30)
      try(private$start_worker(), silent = TRUE)
    },

    drain = function() {
      if (is.null(private$worker_) || private$forked()) return(invisible(NULL))
      if (private$worker_$is_alive()) {
        try(private$worker_$drain(), silent = TRUE)
      } else {
        private$maybe_restart_worker()
      }
      invisible(NULL)
    },

    send_safely = function(type, payload = list()) {
      if (is.null(private$worker_) || !private$worker_$is_alive()) return(invisible(FALSE))
      ok <- tryCatch({
        private$worker_$send(type, payload)
        TRUE
      }, error = function(e) FALSE)
      invisible(ok)
    },

    notify_tracker = function(old) {
      try(private$flag_tracker_$notify_store_update(
        old,
        private$store_$all_flags(),
        eval_fn = function(key, ctx) private$evaluate_internal(key, ctx, NULL)$detail$value
      ), silent = TRUE)
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
        private$restart_delay_ <- 1
        private$notify_tracker(old)
      } else if (identical(type, "Ready")) {
        if (isTRUE(payload$initialized) || isTRUE(payload$offline) || isTRUE(payload$fail_safe)) {
          private$initialized_ <- TRUE
        }
        if (isTRUE(payload$fail_safe)) private$data_status_ <- "off"
      } else if (identical(type, "Status")) {
        private$data_status_ <- payload$state %||% private$data_status_
      } else if (identical(type, "Log")) {
        # Never log payload that might contain keys; worker already redacts
        message(sprintf("[ld:%s] %s", payload$level %||% "info",
                        .ld_redact_secret(payload$message %||% "", private$config_$sdk_key)))
      } else if (identical(type, "Fatal")) {
        warning(.ld_redact_secret(payload$message %||% "worker fatal", private$config_$sdk_key),
                call. = FALSE)
      }
    },

    variation_impl = function(key, context, default, with_reasons) {
      private$check_fork()
      private$drain()
      res <- tryCatch(
        private$evaluate_internal(key, context, default, collect_prereqs = TRUE),
        error = function(e) {
          list(
            detail = evaluation_detail(default, NULL, list(kind = "ERROR", errorKind = "EXCEPTION")),
            flag = NULL, prereq_events = list(), send_event = FALSE
          )
        }
      )
      try(private$send_eval_events(key, context, default, res, with_reasons), silent = TRUE)
      res$detail
    },

    send_eval_events = function(key, context, default, res, with_reasons) {
      if (!isTRUE(res$send_event) || !private$events_enabled() || private$forked()) return()
      now <- round(as.numeric(Sys.time()) * 1000)
      records <- list()
      for (pe in res$prereq_events) {
        records[[length(records) + 1L]] <- .ld_eval_event_record(
          pe$flag$key, context, pe$flag, pe$detail, NULL, with_reasons,
          prereq_of = pe$prereq_of, timestamp = now
        )
      }
      records[[length(records) + 1L]] <- .ld_eval_event_record(
        key, context, res$flag, res$detail, default, with_reasons, timestamp = now
      )
      private$send_safely("Events", list(events = records))
    },

    # Returns list(detail, flag, prereq_events, send_event).
    evaluate_internal = function(key, context, default, collect_prereqs = FALSE) {
      out <- function(detail, flag = NULL, prereqs = list(), send_event = FALSE) {
        list(detail = detail, flag = flag, prereq_events = prereqs, send_event = send_event)
      }
      err <- function(kind) evaluation_detail(default, NULL, list(kind = "ERROR", errorKind = kind))
      if (!identical(Sys.getpid(), private$owner_pid_)) return(out(err("CLIENT_NOT_READY")))
      key_ok <- .ld_valid_key(key)
      if (!isTRUE(private$initialized_) || !private$store_$is_initialized()) {
        return(out(err("CLIENT_NOT_READY"), send_event = key_ok))
      }
      if (!.ld_is_context(context) || !context_valid(context)) {
        return(out(err("USER_NOT_SPECIFIED")))
      }
      flag <- if (key_ok) private$store_$get_flag(key) else NULL
      if (is.null(flag) || isTRUE(flag$deleted)) {
        return(out(err("FLAG_NOT_FOUND"), send_event = key_ok))
      }
      prereq_env <- if (isTRUE(collect_prereqs)) new.env(parent = emptyenv()) else NULL
      detail <- private$evaluate_flag(flag, context, default, prereq_env)
      out(detail, flag, if (is.null(prereq_env)) list() else prereq_env$events %||% list(), TRUE)
    },

    evaluate_flag = function(flag, context, default, prereq_env = NULL) {
      # Hooks observe the evaluation; they cannot substitute the context.
      eval_fn <- function(series_ctx) {
        ld_evaluate_flag(
          flag,
          context,
          default,
          get_flag = function(k) private$store_$get_flag(k),
          get_segment = function(k) private$store_$get_segment(k),
          prereq_events = prereq_env
        )
      }
      if (length(private$hooks_) == 0L) {
        return(eval_fn(list(context = context, flag_key = flag$key)))
      }
      .ld_with_hooks(private$hooks_, list(context = context, flag_key = flag$key), eval_fn)
    },

    all_flags_state_impl = function(context, client_side_only, with_reasons,
                                    details_only_for_tracked_flags) {
      if (!.ld_is_context(context) || !context_valid(context)) {
        return(FeatureFlagsState$new(FALSE))
      }
      if (!isTRUE(private$initialized_) || !private$store_$is_initialized()) {
        return(FeatureFlagsState$new(FALSE))
      }
      state <- FeatureFlagsState$new(TRUE)
      flags <- private$store_$all_flags()
      now_ms <- round(as.numeric(Sys.time()) * 1000)
      for (key in names(flags)) {
        flag <- flags[[key]]
        if (isTRUE(flag$deleted)) next
        if (isTRUE(client_side_only) && !isTRUE(flag$clientSide) && !isTRUE(flag$clientSideAvailability$usingEnvironmentId)) next
        detail <- private$evaluate_flag(flag, context, NULL)
        track_events <- isTRUE(flag$trackEvents)
        track_reason <- isTRUE(detail$reason$inExperiment)
        rk <- detail$reason$kind %||% ""
        if (identical(rk, "FALLTHROUGH") && isTRUE(flag$trackEventsFallthrough)) {
          track_events <- TRUE
          track_reason <- TRUE
        }
        if (identical(rk, "RULE_MATCH")) {
          ri <- detail$reason$ruleIndex
          rules <- flag$rules %||% list()
          if (!is.null(ri) && !is.na(ri) && ri >= 0L && ri < length(rules) &&
              isTRUE(rules[[ri + 1L]]$trackEvents)) {
            track_events <- TRUE
            track_reason <- TRUE
          }
        }
        debug_until <- flag$debugEventsUntilDate
        omit_details <- FALSE
        if (isTRUE(details_only_for_tracked_flags)) {
          if (!track_events && !track_reason &&
              (is.null(debug_until) || !(is.numeric(debug_until) && debug_until > now_ms))) {
            omit_details <- TRUE
          }
        }
        reason <- NULL
        if ((isTRUE(with_reasons) || track_reason) && !omit_details) {
          reason <- detail$reason
        }
        state$add_flag(
          key,
          detail$value,
          variation = detail$variation_index,
          version = if (!omit_details) flag$version else NULL,
          reason = reason,
          track_events = track_events,
          track_reason = track_reason,
          debug_events_until_date = if (!omit_details) debug_until else NULL
        )
      }
      state
    }
  )
)

.ld_is_context <- function(x) inherits(x, "ld_context")

.ld_redact_secret <- function(text, secret) {
  if (!is.character(secret) || length(secret) != 1L || is.na(secret) || nchar(secret) < 4L) {
    return(text)
  }
  gsub(secret, "<redacted>", text, fixed = TRUE)
}

#' Construct an [LdClient]
#' @param config [LdConfig]
#' @param start_wait Optional override for start wait seconds.
#' @export
ld_client <- function(config, start_wait = NULL) {
  LdClient$new(config, start_wait = start_wait)
}
