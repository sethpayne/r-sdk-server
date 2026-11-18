#' FDv2 streaming synchronizer (SSE)
#'
#' @keywords internal
NULL

# Server-side FDv2 streaming path (python-server-sdk + harness mockld)
LD_FDV2_STREAM_PATH <- "/sdk/stream"

#' Read FDv2 SSE until one or more payloads are transferred
#'
#' Uses httr2 connection streaming so long-lived SSE does not block forever on
#' `req_perform`. By default (`until_first_payload = TRUE`) returns after the
#' first `payload-transferred`. The worker may set `until_first_payload = FALSE`
#' and supply `on_changeset` to process multiple payloads on one connection.
#'
#' @param config Config list.
#' @param selector Current selector list or NULL.
#' @param on_changeset function(cs) called for completed change sets.
#' @param should_stop function() -> logical
#' @param until_first_payload logical; if TRUE return after first payload.
#' @keywords internal
fdv2_stream_once <- function(config, selector = NULL, on_changeset = NULL,
                             should_stop = function() FALSE,
                             until_first_payload = TRUE) {
  base <- sub("/$", "", config$stream_uri %||% "https://stream.launchdarkly.com")
  url <- paste0(base, LD_FDV2_STREAM_PATH)
  query <- list()
  if (!is.null(selector) && nzchar(selector$state %||% "")) {
    query$basis <- selector$state
  }
  if (!is.null(config$payload_filter_key) && nzchar(config$payload_filter_key)) {
    query$filter <- config$payload_filter_key
  }

  req <- httr2::request(url) |>
    httr2::req_method("GET") |>
    httr2::req_headers(
      Authorization = config$sdk_key,
      `User-Agent` = LD_USER_AGENT,
      Accept = "text/event-stream"
    ) |>
    httr2::req_timeout(config$read_timeout %||% 300)

  tags <- .ld_tags_header_value(config)
  if (!is.null(tags)) {
    req <- httr2::req_headers(req, `X-LaunchDarkly-Tags` = tags)
  }
  if (length(query)) req <- httr2::req_url_query(req, !!!query)

  resp <- tryCatch(httr2::req_perform_connection(req), error = function(e) e)
  if (inherits(resp, "error")) {
    resp2 <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (inherits(resp2, "error")) {
      return(list(ok = FALSE, error = conditionMessage(resp), fallback_to_fdv1 = FALSE))
    }
    return(.ld_stream_parse_body_response(resp2, on_changeset))
  }
  on.exit(try(close(resp), silent = TRUE), add = TRUE)

  headers <- httr2::resp_headers(resp)
  names(headers) <- tolower(names(headers))
  fallback <- identical(tolower(as.character(headers[[LD_FD_FALLBACK_HEADER]] %||% "")), "true")
  env_id <- headers[[LD_ENVID_HEADER]]
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    return(list(
      ok = FALSE, error = sprintf("stream HTTP %s", status), status = status,
      fallback_to_fdv1 = fallback, headers = headers
    ))
  }
  if (fallback) {
    return(list(ok = FALSE, error = "fdv1Fallback", fallback_to_fdv1 = TRUE, headers = headers))
  }

  deadline <- proc.time()[["elapsed"]] + as.numeric(config$read_timeout %||% 300)
  events_acc <- list()
  last_ok <- NULL
  got_any_payload <- FALSE

  flush_payload <- function() {
    cs <- fdv2_polling_payload_to_changeset(list(events = events_acc))
    events_acc <<- list()
    if (!is.null(cs$error)) return(cs)
    if (is.function(on_changeset)) on_changeset(cs)
    last_ok <<- list(ok = TRUE, changeset = cs, environment_id = env_id, fallback_to_fdv1 = FALSE)
    got_any_payload <<- TRUE
    cs
  }

  repeat {
    if (isTRUE(should_stop()) || proc.time()[["elapsed"]] > deadline) break

    ev <- tryCatch(httr2::resp_stream_sse(resp), error = function(e) e)
    if (inherits(ev, "error")) {
      return(list(ok = FALSE, error = conditionMessage(ev), fallback_to_fdv1 = FALSE))
    }
    if (is.null(ev)) {
      if (isTRUE(httr2::resp_stream_is_complete(resp))) break
      Sys.sleep(0.01)
      next
    }

    event_name <- ev$type %||% "message"
    # Heartbeats / comments produce empty data — ignore
    if (identical(event_name, "message") && !nzchar(ev$data %||% "")) next
    if (identical(event_name, "ping") || identical(event_name, "heartbeat")) next

    data <- NULL
    if (nzchar(ev$data %||% "")) {
      data <- tryCatch(
        jsonlite::fromJSON(ev$data, simplifyVector = FALSE),
        error = function(e) {
          # Malformed JSON → discard in-flight payload and reconnect
          structure(list(.parse_error = TRUE), class = "ld_sse_parse_error")
        }
      )
    }
    if (inherits(data, "ld_sse_parse_error")) {
      return(list(ok = FALSE, error = "malformed SSE JSON", reconnect = TRUE, fallback_to_fdv1 = FALSE))
    }

    # Unknown events are ignored (still advance past them)
    known <- event_name %in% c(
      LD_EVENT_SERVER_INTENT, LD_EVENT_PUT_OBJECT, LD_EVENT_DELETE_OBJECT,
      LD_EVENT_PAYLOAD_TRANSFERRED, "goodbye", "error"
    )
    if (!known) next

    if (identical(event_name, "goodbye")) {
      return(list(ok = FALSE, error = "goodbye", reconnect = TRUE, fallback_to_fdv1 = FALSE))
    }
    if (identical(event_name, "error")) {
      # Protocol error event — discard and reconnect / fallback
      return(list(ok = FALSE, error = "stream error event", reconnect = TRUE, fallback_to_fdv1 = FALSE))
    }

    events_acc[[length(events_acc) + 1L]] <- list(event = event_name, data = data)

    if (identical(event_name, LD_EVENT_PAYLOAD_TRANSFERRED)) {
      cs <- flush_payload()
      if (!is.null(cs$error)) {
        return(list(ok = FALSE, error = cs$error, fallback_to_fdv1 = FALSE))
      }
      if (isTRUE(until_first_payload)) break
    }
  }

  # Finite body servers (webfakes) may complete without framing the last event
  if (!got_any_payload && isTRUE(httr2::resp_stream_is_complete(resp))) {
    try(close(resp), silent = TRUE)
    on.exit(NULL)
    resp2 <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (!inherits(resp2, "error")) {
      return(.ld_stream_parse_body_response(resp2, on_changeset))
    }
  }

  if (!is.null(last_ok)) return(last_ok)
  list(ok = FALSE, error = "stream incomplete", fallback_to_fdv1 = FALSE)
}

#' @keywords internal
.ld_stream_parse_body_response <- function(resp, on_changeset = NULL) {
  headers <- httr2::resp_headers(resp)
  names(headers) <- tolower(names(headers))
  fallback <- identical(tolower(as.character(headers[[LD_FD_FALLBACK_HEADER]] %||% "")), "true")
  env_id <- headers[[LD_ENVID_HEADER]]
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    return(list(ok = FALSE, error = sprintf("stream HTTP %s", status), status = status,
                fallback_to_fdv1 = fallback, headers = headers))
  }
  if (fallback) {
    return(list(ok = FALSE, error = "fdv1Fallback", fallback_to_fdv1 = TRUE, headers = headers))
  }

  body <- httr2::resp_body_string(resp)
  events_acc <- list()
  current_event <- NULL
  data_lines <- character()
  last_ok <- NULL

  flush_event <- function() {
    if (is.null(current_event) || length(data_lines) == 0L) {
      current_event <<- NULL
      data_lines <<- character()
      return(NULL)
    }
    raw <- paste(data_lines, collapse = "\n")
    data <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    ev <- list(event = current_event, data = data)
    current_event <<- NULL
    data_lines <<- character()
    ev
  }

  for (line in strsplit(body, "\n", fixed = TRUE)[[1]]) {
    if (identical(line, "")) {
      ev <- flush_event()
      if (is.null(ev)) next
      if (identical(ev$event, "goodbye")) {
        return(list(ok = FALSE, error = "goodbye", reconnect = TRUE, fallback_to_fdv1 = FALSE))
      }
      events_acc[[length(events_acc) + 1L]] <- ev
      if (identical(ev$event, LD_EVENT_PAYLOAD_TRANSFERRED)) {
        cs <- fdv2_polling_payload_to_changeset(list(events = events_acc))
        events_acc <- list()
        if (!is.null(cs$error)) {
          return(list(ok = FALSE, error = cs$error, fallback_to_fdv1 = FALSE))
        }
        if (is.function(on_changeset)) on_changeset(cs)
        last_ok <- list(ok = TRUE, changeset = cs, environment_id = env_id, fallback_to_fdv1 = FALSE)
      }
    } else if (startsWith(line, "event:")) {
      current_event <- trimws(sub("^event:\\s*", "", line))
    } else if (startsWith(line, "data:")) {
      data_lines <- c(data_lines, sub("^data:\\s?", "", line))
    }
  }
  ev <- flush_event()
  if (!is.null(ev)) {
    events_acc[[length(events_acc) + 1L]] <- ev
    if (identical(ev$event, LD_EVENT_PAYLOAD_TRANSFERRED)) {
      cs <- fdv2_polling_payload_to_changeset(list(events = events_acc))
      if (!is.null(cs$error)) {
        return(list(ok = FALSE, error = cs$error, fallback_to_fdv1 = FALSE))
      }
      if (is.function(on_changeset)) on_changeset(cs)
      last_ok <- list(ok = TRUE, changeset = cs, environment_id = env_id, fallback_to_fdv1 = FALSE)
    }
  }
  if (!is.null(last_ok)) return(last_ok)
  list(ok = FALSE, error = "stream incomplete", fallback_to_fdv1 = FALSE)
}
