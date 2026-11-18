#' FDv2 streaming synchronizer (SSE)
#'
#' @keywords internal
NULL

# Server-side FDv2 streaming path (python-server-sdk + harness mockld)
LD_FDV2_STREAM_PATH <- "/sdk/stream"

#' Parse raw HTTP header block from curl multi `done` / handle_data
#' @keywords internal
.ld_parse_raw_http_headers <- function(raw_headers) {
  if (is.null(raw_headers) || !length(raw_headers)) {
    return(list(status = NA_integer_, headers = list()))
  }
  txt <- if (is.raw(raw_headers)) rawToChar(raw_headers) else paste(raw_headers, collapse = "")
  lines <- strsplit(txt, "\r\n", fixed = TRUE)[[1]]
  status <- NA_integer_
  if (length(lines) >= 1L && grepl("^HTTP/", lines[[1]])) {
    parts <- strsplit(lines[[1]], "\\s+")[[1]]
    if (length(parts) >= 2L) status <- suppressWarnings(as.integer(parts[[2]]))
  }
  headers <- list()
  if (length(lines) >= 2L) {
    for (line in lines[-1]) {
      if (!nzchar(line)) next
      if (!grepl(":", line, fixed = TRUE)) next
      key <- tolower(sub(":.*$", "", line))
      val <- sub("^[^:]*:\\s*", "", line)
      headers[[key]] <- val
    }
  }
  list(status = status, headers = headers)
}

#' Parse buffered SSE text into event list (consumes complete events)
#' @return list(events, rest)
#' @keywords internal
.ld_sse_split_buffer <- function(text) {
  events <- list()
  # Events end at a blank line
  while (grepl("\n\n|\r\n\r\n", text)) {
    m <- regexpr("\r?\n\r?\n", text)
    if (m < 1) break
    block <- substr(text, 1L, m - 1L)
    text <- substr(text, m + attr(m, "match.length"), nchar(text))
    ev <- .ld_sse_parse_block(block)
    if (!is.null(ev)) events[[length(events) + 1L]] <- ev
  }
  list(events = events, rest = text)
}

#' @keywords internal
.ld_sse_parse_block <- function(block) {
  lines <- strsplit(block, "\r\n|\n|\r")[[1]]
  type <- ""
  data <- ""
  for (line in lines) {
    if (!nzchar(line) || startsWith(line, ":")) next
    if (startsWith(line, "event:")) {
      type <- trimws(sub("^event:\\s?", "", line))
    } else if (startsWith(line, "data:")) {
      data <- paste0(data, sub("^data:\\s?", "", line), "\n")
    }
  }
  if (!nzchar(data)) return(NULL)
  if (grepl("\n$", data)) data <- substr(data, 1L, nchar(data) - 1L)
  if (!nzchar(type)) type <- "message"
  list(type = type, data = data)
}

#' Read FDv2 SSE until one or more payloads are transferred
#'
#' Uses the curl multi API with a `data` callback so bytes are delivered as they
#' arrive on long-lived eventsource connections. httr2's `resp_stream_sse` is
#' unsuitable here: libcurl blocking reads wait for a full 64KiB chunk (or a
#' request timeout), so open SSE streams never surface the first events in time
#' for contract-service `startWait`.
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
  if (length(query)) {
    url <- paste0(url, "?", paste(
      vapply(names(query), function(n) {
        paste0(utils::URLencode(n, TRUE), "=", utils::URLencode(as.character(query[[n]]), TRUE))
      }, character(1)),
      collapse = "&"
    ))
  }

  hdrs <- c(
    Authorization = config$sdk_key %||% "",
    `User-Agent` = LD_USER_AGENT,
    Accept = "text/event-stream"
  )
  tags <- .ld_tags_header_value(config)
  if (!is.null(tags)) hdrs[["X-LaunchDarkly-Tags"]] <- tags

  default_timeout <- if (isTRUE(until_first_payload)) 4 else 300
  deadline <- proc.time()[["elapsed"]] + as.numeric(
    config$stream_read_timeout %||% config$read_timeout %||% default_timeout
  )
  if (isTRUE(until_first_payload)) {
    deadline <- min(deadline, proc.time()[["elapsed"]] + 4)
  }

  buf_txt <- ""
  events_acc <- list()
  last_ok <- NULL
  got_any_payload <- FALSE
  http_status <- NA_integer_
  http_headers <- list()
  finished <- FALSE
  fail_msg <- NULL
  env_id <- NULL

  flush_payload <- function() {
    cs <- fdv2_polling_payload_to_changeset(list(events = events_acc))
    events_acc <<- list()
    if (!is.null(cs$error)) return(cs)
    if (is.function(on_changeset)) on_changeset(cs)
    last_ok <<- list(ok = TRUE, changeset = cs, environment_id = env_id, fallback_to_fdv1 = FALSE)
    got_any_payload <<- TRUE
    cs
  }

  handle_event <- function(ev) {
    event_name <- ev$type %||% "message"
    if (identical(event_name, "message") && !nzchar(ev$data %||% "")) return(NULL)
    if (identical(event_name, "ping") || identical(event_name, "heartbeat")) return(NULL)

    # Unknown event names are ignored before parsing: their bodies need not be JSON.
    known <- event_name %in% c(
      LD_EVENT_SERVER_INTENT, LD_EVENT_PUT_OBJECT, LD_EVENT_DELETE_OBJECT,
      LD_EVENT_PAYLOAD_TRANSFERRED, "goodbye", "error"
    )
    if (!known) return(NULL)

    data <- NULL
    if (nzchar(ev$data %||% "")) {
      data <- tryCatch(
        jsonlite::fromJSON(ev$data, simplifyVector = FALSE),
        error = function(e) structure(list(.parse_error = TRUE), class = "ld_sse_parse_error")
      )
    }
    if (inherits(data, "ld_sse_parse_error")) {
      return(list(ok = FALSE, error = "malformed SSE JSON", reconnect = TRUE, fallback_to_fdv1 = FALSE))
    }

    if (identical(event_name, "goodbye")) {
      return(list(ok = FALSE, error = "goodbye", reconnect = TRUE, fallback_to_fdv1 = FALSE))
    }
    if (identical(event_name, "error")) {
      # Protocol error: discard the in-flight payload and keep the connection
      # (harness can discard partial / full events on errors).
      events_acc <<- list()
      return(NULL)
    }

    events_acc[[length(events_acc) + 1L]] <<- list(event = event_name, data = data)
    if (identical(event_name, LD_EVENT_PAYLOAD_TRANSFERRED)) {
      cs <- flush_payload()
      if (!is.null(cs$error)) {
        return(list(ok = FALSE, error = cs$error, fallback_to_fdv1 = FALSE))
      }
      if (isTRUE(until_first_payload)) {
        return(list(.stop = TRUE))
      }
    }
    NULL
  }

  state <- new.env(parent = emptyenv())
  state$pending <- list()

  h <- curl::new_handle(url = url)
  do.call(curl::handle_setheaders, c(list(h), as.list(hdrs)))
  curl::handle_setopt(h, connecttimeout = as.integer(config$connect_timeout %||% 10),
                      noprogress = TRUE, timeout = 0)
  pool <- curl::new_pool()
  curl::multi_add(
    h,
    pool = pool,
    done = function(res) {
      http_status <<- res$status_code %||% NA_integer_
      parsed <- .ld_parse_raw_http_headers(res$headers)
      if (length(parsed$headers)) http_headers <<- parsed$headers
      if (is.na(http_status) && !is.na(parsed$status)) http_status <<- parsed$status
      finished <<- TRUE
    },
    fail = function(msg) {
      fail_msg <<- msg
      finished <<- TRUE
    },
    data = function(x, finalize = FALSE) {
      if (!length(x)) return()
      buf_txt <<- paste0(buf_txt, rawToChar(x))
      split <- .ld_sse_split_buffer(buf_txt)
      buf_txt <<- split$rest
      if (length(split$events)) {
        state$pending <- c(state$pending, split$events)
      }
    }
  )

  finish_http <- function() {
    fb <- identical(tolower(as.character(http_headers[[LD_FD_FALLBACK_HEADER]] %||% "")), "true")
    env_id <<- http_headers[[LD_ENVID_HEADER]]
    if (!is.na(http_status) && http_status >= 400) {
      return(list(
        ok = FALSE, error = sprintf("stream HTTP %s", http_status), status = as.integer(http_status),
        fallback_to_fdv1 = fb, headers = http_headers
      ))
    }
    if (isTRUE(fb)) {
      return(list(ok = FALSE, error = "fdv1Fallback", fallback_to_fdv1 = TRUE, headers = http_headers))
    }
    NULL
  }

  early <- NULL
  repeat {
    if (isTRUE(should_stop()) || proc.time()[["elapsed"]] > deadline) break
    if (!is.null(fail_msg)) {
      return(list(ok = FALSE, error = fail_msg, fallback_to_fdv1 = FALSE))
    }

    curl::multi_run(pool = pool, timeout = 0.05)

    # Opportunistically read status from handle once headers arrive
    if (is.na(http_status)) {
      hd <- tryCatch(curl::handle_data(h), error = function(e) NULL)
      if (!is.null(hd) && !is.null(hd$status_code) && !is.na(hd$status_code) && hd$status_code > 0) {
        http_status <- hd$status_code
        parsed <- .ld_parse_raw_http_headers(hd$headers)
        if (length(parsed$headers)) http_headers <- parsed$headers
      }
    }

    # Error responses (401/403) complete immediately — handle after multi_run so
    # we do not race past the done callback into "stream incomplete".
    if (isTRUE(finished) || (!is.na(http_status) && http_status >= 400)) {
      err <- finish_http()
      if (!is.null(err)) {
        try(curl::multi_cancel(h), silent = TRUE)
        return(err)
      }
    }

    pending <- state$pending
    state$pending <- list()
    for (ev in pending) {
      r <- handle_event(ev)
      if (!is.null(r)) {
        if (isTRUE(r$.stop)) {
          early <- last_ok
          break
        }
        try(curl::multi_cancel(h), silent = TRUE)
        return(r)
      }
    }
    if (!is.null(early)) break
    if (isTRUE(finished) && !length(state$pending) && !nzchar(buf_txt)) break
  }

  # Drop the handle from the pool so the connection is released
  try(curl::multi_cancel(h), silent = TRUE)

  if (!is.null(last_ok)) return(last_ok)

  # Finite-body fallback for webfakes / unit tests when multi got nothing
  if (!got_any_payload) {
    req <- httr2::request(url) |>
      httr2::req_method("GET") |>
      httr2::req_headers(!!!as.list(hdrs)) |>
      httr2::req_timeout(config$read_timeout %||% 15)
    resp2 <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (!inherits(resp2, "error")) {
      return(.ld_stream_parse_body_response(resp2, on_changeset))
    }
  }

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
