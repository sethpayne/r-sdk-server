#' SDK configuration
#'
#' @description Create with [ld_config()]. Defaults mirror python-server-sdk
#' where applicable (start wait 5s, poll interval 30s).
#'
#' @export
LdConfig <- R6::R6Class(
  "LdConfig",
  public = list(
    sdk_key = NULL,
    base_uri = "https://app.launchdarkly.com",
    stream_uri = "https://stream.launchdarkly.com",
    events_uri = "https://events.launchdarkly.com",
    offline = FALSE,
    stream = TRUE,
    poll_interval = 30,
    start_wait = 5,
    send_events = TRUE,
    events_capacity = 10000L,
    flush_interval = 5,
    all_attributes_private = FALSE,
    private_attributes = character(),
    application_id = NULL,
    application_version = NULL,
    wrapper_name = NULL,
    wrapper_version = NULL,
    connect_timeout = 10,
    read_timeout = 15,
    initial_reconnect_delay = 1,
    payload_filter_key = NULL,
    diagnostic_opt_out = TRUE,
    omit_anonymous_contexts = FALSE,
    sync_poll_uri = NULL,
    fallback_poll_uri = NULL,
    fallback_stream_uri = NULL,
    poll_init_uris = NULL,
    stream_only = FALSE,
    skip_poll_init = FALSE,
    context_keys_capacity = 1000L,
    context_keys_flush_interval = 300,

    #' @param sdk_key LaunchDarkly SDK key.
    initialize = function(sdk_key = "") {
      self$sdk_key <- as.character(sdk_key)
      invisible(self)
    },

    #' @description One line per option; the SDK key is masked.
    format = function(...) {
      fields <- setdiff(.ld_config_fields(self), "sdk_key")
      key <- if (length(self$sdk_key) == 1L && nzchar(self$sdk_key)) "<redacted>" else "<unset>"
      vals <- vapply(fields, function(f) {
        v <- self[[f]]
        if (is.null(v)) "NULL" else paste(format(v), collapse = ", ")
      }, character(1))
      c("<LdConfig>", paste0("  sdk_key: ", key), paste0("  ", fields, ": ", vals))
    },

    #' @description Print without revealing the SDK key.
    print = function(...) {
      cat(self$format(...), sep = "\n")
      invisible(self)
    },

    #' Compact list for IPC Start (sdk_key included once; never logged).
    to_list = function() {
      list(
        sdk_key = self$sdk_key,
        base_uri = self$base_uri,
        stream_uri = self$stream_uri,
        events_uri = self$events_uri,
        offline = isTRUE(self$offline),
        stream = isTRUE(self$stream),
        poll_interval = self$poll_interval,
        send_events = isTRUE(self$send_events),
        events_capacity = self$events_capacity,
        flush_interval = self$flush_interval,
        all_attributes_private = isTRUE(self$all_attributes_private),
        private_attributes = as.list(self$private_attributes),
        application_id = self$application_id,
        application_version = self$application_version,
        wrapper_name = self$wrapper_name,
        wrapper_version = self$wrapper_version,
        connect_timeout = self$connect_timeout,
        read_timeout = self$read_timeout,
        initial_reconnect_delay = self$initial_reconnect_delay,
        payload_filter_key = self$payload_filter_key,
        diagnostic_opt_out = isTRUE(self$diagnostic_opt_out),
        omit_anonymous_contexts = isTRUE(self$omit_anonymous_contexts),
        sync_poll_uri = self$sync_poll_uri,
        fallback_poll_uri = self$fallback_poll_uri,
        fallback_stream_uri = self$fallback_stream_uri,
        poll_init_uris = as.list(self$poll_init_uris %||% list()),
        stream_only = isTRUE(self$stream_only),
        skip_poll_init = isTRUE(self$skip_poll_init),
        context_keys_capacity = self$context_keys_capacity,
        context_keys_flush_interval = self$context_keys_flush_interval
      )
    }
  )
)

#' Create an [LdConfig]
#'
#' @param sdk_key SDK key (ignored when `offline = TRUE`).
#' @param ... Named overrides for [LdConfig] public fields.
#' @export
ld_config <- function(sdk_key = "", ...) {
  cfg <- LdConfig$new(sdk_key)
  dots <- list(...)
  fields <- .ld_config_fields(cfg)
  for (nm in names(dots)) {
    if (!nm %in% fields) {
      warning(sprintf("Unknown config option '%s' ignored", nm), call. = FALSE)
      next
    }
    val <- dots[[nm]]
    if (nm %in% .ld_config_numeric_fields &&
        !(is.numeric(val) && length(val) == 1L && !is.na(val) && val >= 0)) {
      warning(sprintf(
        "Config option '%s' must be a single non-negative number; using default %s",
        nm, format(cfg[[nm]])
      ), call. = FALSE)
      next
    }
    if (nm %in% .ld_config_logical_fields && !(is.logical(val) && length(val) == 1L && !is.na(val))) {
      warning(sprintf(
        "Config option '%s' must be TRUE or FALSE; using default %s", nm, format(cfg[[nm]])
      ), call. = FALSE)
      next
    }
    cfg[[nm]] <- val
  }
  cfg
}

.ld_config_numeric_fields <- c(
  "poll_interval", "start_wait", "events_capacity", "flush_interval", "connect_timeout",
  "read_timeout", "initial_reconnect_delay", "context_keys_capacity", "context_keys_flush_interval"
)

.ld_config_logical_fields <- c(
  "offline", "stream", "send_events", "all_attributes_private", "diagnostic_opt_out",
  "omit_anonymous_contexts", "stream_only", "skip_poll_init"
)

.ld_config_fields <- function(cfg) {
  nms <- setdiff(ls(cfg, sorted = TRUE), c(".__enclos_env__", "clone"))
  nms[!vapply(nms, function(n) is.function(cfg[[n]]), logical(1))]
}
