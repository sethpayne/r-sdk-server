#' FDv2 streaming synchronizer (SSE)
#'
#' @keywords internal
NULL

LD_FDV2_STREAM_PATH <- "/meval/v2/stream"

#' Read FDv2 SSE stream until one payload-transferred or error/goodbye
#'
#' Uses httr2 streaming. For the worker loop, prefer short-lived reads with
#' reconnect backoff (handled by caller).
#'
#' @param config Config list.
#' @param selector Current selector list or NULL.
#' @param on_changeset function(cs) called for completed change sets.
#' @param should_stop function() -> logical
#' @keywords internal
fdv2_stream_once <- function(config, selector = NULL, on_changeset = NULL, should_stop = function() FALSE) {
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

  if (length(query)) req <- httr2::req_url_query(req, !!!query)

  # Accumulate SSE events into an FDv2 polling-shaped payload
  events_acc <- list()
  current_event <- NULL
  data_lines <- character()
  fallback <- FALSE
  env_id <- NULL
  err <- NULL

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

  # Blocking perform with callback — httr2 1.0+ req_perform_stream
  parse_line <- function(line) {
    if (identical(line, "")) {
      ev <- flush_event()
      if (!is.null(ev)) {
        events_acc[[length(events_acc) + 1L]] <<- ev
        if (identical(ev$event, LD_EVENT_PAYLOAD_TRANSFERRED) ||
            identical(ev$event, "goodbye") ||
            identical(ev$event, "error")) {
          return("done")
        }
      }
      return(NULL)
    }
    if (startsWith(line, "event:")) {
      current_event <<- trimws(sub("^event:\\s*", "", line))
    } else if (startsWith(line, "data:")) {
      data_lines <<- c(data_lines, sub("^data:\\s?", "", line))
    }
    NULL
  }

  # Fallback: single HTTP GET that may not be true SSE in tests — webfakes can
  # return a complete SSE body at once.
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, error = conditionMessage(resp), fallback_to_fdv1 = FALSE))
  }

  headers <- httr2::resp_headers(resp)
  names(headers) <- tolower(names(headers))
  fallback <- identical(tolower(as.character(headers[[LD_FD_FALLBACK_HEADER]] %||% "")), "true")
  env_id <- headers[[LD_ENVID_HEADER]]
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    return(list(ok = FALSE, error = sprintf("stream HTTP %s", status), status = status,
                fallback_to_fdv1 = fallback, headers = headers))
  }

  body <- httr2::resp_body_string(resp)
  for (line in strsplit(body, "\n", fixed = TRUE)[[1]]) {
    if (isTRUE(should_stop())) break
    parse_line(line)
  }
  # trailing event without blank line
  ev <- flush_event()
  if (!is.null(ev)) events_acc[[length(events_acc) + 1L]] <- ev

  if (fallback) {
    return(list(ok = FALSE, error = "fdv1Fallback", fallback_to_fdv1 = TRUE, headers = headers))
  }

  cs <- fdv2_polling_payload_to_changeset(list(events = events_acc))
  if (!is.null(cs$error)) {
    # goodbye alone is not a change set
    if (any(vapply(events_acc, function(e) identical(e$event, "goodbye"), logical(1)))) {
      return(list(ok = FALSE, error = "goodbye", reconnect = TRUE, fallback_to_fdv1 = FALSE))
    }
    return(list(ok = FALSE, error = cs$error, fallback_to_fdv1 = FALSE))
  }
  if (is.function(on_changeset)) on_changeset(cs)
  list(ok = TRUE, changeset = cs, environment_id = env_id, fallback_to_fdv1 = FALSE)
}
