#' Analytics event buffer and formatting (inline context schema)
#'
#' @keywords internal
NULL

#' @export
EventBuffer <- R6::R6Class(
  "EventBuffer",
  public = list(
    initialize = function(capacity = 10000L, all_private = FALSE, private_attrs = character()) {
      private$capacity_ <- as.integer(capacity)
      private$all_private_ <- isTRUE(all_private)
      private$private_attrs_ <- as.character(private_attrs)
      private$events_ <- list()
      invisible(self)
    },

    size = function() length(private$events_),

    add_index = function(context) {
      private$push(list(
        kind = "index",
        creationDate = private$now_ms(),
        context = private$redact_context(context)
      ))
    },

    add_identify = function(context) {
      private$push(list(
        kind = "identify",
        creationDate = private$now_ms(),
        context = private$redact_context(context)
      ))
    },

    add_custom = function(event_name, context, data = NULL, metric_value = NULL) {
      ev <- list(
        kind = "custom",
        key = event_name,
        creationDate = private$now_ms(),
        context = private$redact_context(context)
      )
      if (!is.null(data)) ev$data <- data
      if (!is.null(metric_value)) ev$metricValue <- metric_value
      private$push(ev)
    },

    add_feature = function(flag_key, context, value, default, version = NULL,
                           variation = NULL, track_events = FALSE, reason = NULL,
                           prereq_of = NULL) {
      ev <- list(
        kind = "feature",
        key = flag_key,
        creationDate = private$now_ms(),
        context = private$redact_context(context),
        value = value,
        default = default
      )
      if (!is.null(version)) ev$version <- version
      if (!is.null(variation)) ev$variation <- variation
      if (isTRUE(track_events) && !is.null(reason)) ev$reason <- reason
      if (!is.null(prereq_of)) ev$prereqOf <- prereq_of
      private$push(ev)
    },

    add_summary = function(start_ms, end_ms, counters) {
      private$push(list(
        kind = "summary",
        startDate = start_ms,
        endDate = end_ms,
        features = counters
      ))
    },

    drain = function() {
      out <- private$events_
      private$events_ <- list()
      out
    }
  ),
  private = list(
    capacity_ = 10000L,
    all_private_ = FALSE,
    private_attrs_ = character(),
    events_ = NULL,
    now_ms = function() round(as.numeric(Sys.time()) * 1000),
    push = function(ev) {
      if (length(private$events_) >= private$capacity_) {
        # Drop oldest
        private$events_ <- private$events_[-1L]
      }
      private$events_[[length(private$events_) + 1L]] <- ev
      invisible(NULL)
    },
    redact_context = function(context_list) {
      # context_list is already JSON-shaped from context_to_list
      if (isTRUE(private$all_private_)) {
        if (identical(context_list$kind, "multi")) {
          for (nm in names(context_list)) {
            if (identical(nm, "kind")) next
            context_list[[nm]] <- private$redact_single(context_list[[nm]], keep_kind = FALSE)
          }
          return(context_list)
        }
        return(private$redact_single(context_list, keep_kind = TRUE))
      }
      if (length(private$private_attrs_) == 0L) return(context_list)
      # Attribute-level redaction: remove matching top-level custom attrs
      private$strip_attrs(context_list, private$private_attrs_)
    },
    redact_single = function(obj, keep_kind) {
      out <- list(key = obj$key %||% "", kind = if (keep_kind) obj$kind %||% "user" else NULL)
      out <- out[!vapply(out, is.null, logical(1))]
      if (isTRUE(obj$anonymous)) out$anonymous <- TRUE
      out
    },
    strip_attrs = function(obj, attrs) {
      if (identical(obj$kind, "multi")) {
        for (nm in names(obj)) {
          if (identical(nm, "kind")) next
          obj[[nm]] <- private$strip_attrs_single(obj[[nm]], attrs)
        }
        return(obj)
      }
      private$strip_attrs_single(obj, attrs)
    },
    strip_attrs_single = function(obj, attrs) {
      for (a in attrs) {
        # Simple name or /path — strip top-level name
        name <- if (startsWith(a, "/")) sub("^/", "", strsplit(a, "/", fixed = TRUE)[[1]][[1]]) else a
        if (name %in% c("key", "kind", "anonymous")) next
        obj[[name]] <- NULL
      }
      # Also honor context _meta privateAttributes
      meta_priv <- obj[["_meta"]]$privateAttributes
      if (!is.null(meta_priv)) {
        for (a in meta_priv) {
          name <- if (startsWith(a, "/")) sub("^/", "", a) else a
          if (!name %in% c("key", "kind", "anonymous")) obj[[name]] <- NULL
        }
      }
      obj
    }
  )
)

#' POST events to LaunchDarkly events URI
#' @keywords internal
ld_post_events <- function(config, events) {
  if (!isTRUE(config$send_events) || length(events) == 0L) {
    return(list(ok = TRUE, count = 0L))
  }
  base <- sub("/$", "", config$events_uri %||% "https://events.launchdarkly.com")
  url <- paste0(base, "/bulk")
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = config$sdk_key,
      `User-Agent` = LD_USER_AGENT,
      `Content-Type` = "application/json",
      `X-LaunchDarkly-Event-Schema` = "4"
    ) |>
    httr2::req_body_raw(
      charToRaw(jsonlite::toJSON(events, auto_unbox = TRUE, null = "null")),
      "application/json"
    ) |>
    httr2::req_timeout(config$read_timeout %||% 15)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, error = conditionMessage(resp)))
  }
  list(ok = httr2::resp_status(resp) < 400, status = httr2::resp_status(resp), count = length(events))
}
