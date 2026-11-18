# Scriptable mock LaunchDarkly service (webfakes child process).
#
# Handlers run in the webfakes subprocess, so they may only use `req`, `res`
# and `req$app$locals` — never variables from the test environment.
#
#   mock <- local_mock_ld()
#   mock$script("/sdk/poll", list(status = 200, body = fdv2_poll_body(flags)))
#   mock$url, mock$requests(), mock$reset()
#
# A script is a queue of responses for a path; each request consumes one and
# the last one repeats. A response has `status`, `headers`, `body`, `delay`.

.ld_mock_app <- function() {
  app <- webfakes::new_app()
  app$locals$log <- list()
  app$locals$scripts <- list()

  serve <- function(req, res) {
    locals <- req$app$locals
    body <- if (is.raw(req$.body)) rawToChar(req$.body) else ""
    locals$log[[length(locals$log) + 1L]] <- list(
      method = req$method,
      path = req$path,
      query = req$query_string,
      headers = as.list(req$headers),
      body = body,
      time = as.numeric(Sys.time())
    )
    queue <- locals$scripts[[req$path]]
    if (is.null(queue) || length(queue) == 0L) {
      if (identical(req$path, "/bulk")) return(res$set_status(202L)$send(""))
      return(res$set_status(404L)$send("no script"))
    }
    spec <- queue[[1L]]
    if (length(queue) > 1L) locals$scripts[[req$path]] <- queue[-1L]
    if (!is.null(spec$delay)) Sys.sleep(as.numeric(spec$delay))
    for (h in names(spec$headers)) res$set_header(h, spec$headers[[h]])
    res$set_status(as.integer(if (is.null(spec$status)) 200L else spec$status))
    res$send(if (is.null(spec$body)) "" else spec$body)
  }
  environment(serve) <- baseenv()
  app$get("/sdk/poll", serve)
  app$get("/sdk/stream", serve)
  app$post("/bulk", serve)
  app$get("/alt/sdk/poll", serve)

  handler <- function(f) {
    environment(f) <- baseenv()
    f
  }
  app$post("/__script", handler(function(req, res) {
    spec <- jsonlite::fromJSON(rawToChar(req$.body), simplifyVector = FALSE)
    req$app$locals$scripts[[spec$path]] <- spec$responses
    res$send_json(list(ok = TRUE), auto_unbox = TRUE)
  }))
  app$get("/__log", handler(function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send(as.character(jsonlite::toJSON(req$app$locals$log, auto_unbox = TRUE, null = "null")))
  }))
  app$post("/__reset", handler(function(req, res) {
    req$app$locals$log <- list()
    res$send_json(list(ok = TRUE), auto_unbox = TRUE)
  }))
  app
}

local_mock_ld <- function(.env = parent.frame()) {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  proc <- webfakes::new_app_process(.ld_mock_app())
  withr::defer(proc$stop(), envir = .env)
  url <- sub("/$", "", proc$url())
  ctl <- function(path, body = NULL) {
    req <- httr2::request(paste0(url, path))
    if (!is.null(body)) {
      req <- httr2::req_body_raw(req, as.character(jsonlite::toJSON(
        body, auto_unbox = TRUE, null = "null", digits = NA
      )), "application/json")
    }
    httr2::resp_body_string(httr2::req_perform(req))
  }
  list(
    url = url,
    script = function(path, ...) {
      responses <- list(...)
      ctl("/__script", list(path = path, responses = responses))
      invisible(NULL)
    },
    requests = function(path = NULL) {
      log <- jsonlite::fromJSON(ctl("/__log"), simplifyVector = FALSE)
      if (!is.null(path)) log <- Filter(function(r) identical(r$path, path), log)
      log
    },
    reset = function() invisible(ctl("/__reset", list()))
  )
}

# Poll until `cond()` is TRUE (or time out); returns the final value.
wait_for <- function(cond, timeout = 10, interval = 0.05) {
  deadline <- Sys.time() + timeout
  repeat {
    ok <- isTRUE(tryCatch(cond(), error = function(e) FALSE))
    if (ok || Sys.time() > deadline) return(ok)
    Sys.sleep(interval)
  }
}

# Header lookup on a logged request (webfakes lower-cases names).
req_header <- function(r, name) {
  h <- r$headers
  h[[tolower(name)]] %||% h[[name]]
}

# Decoded /bulk payloads (list of event lists) received by the mock.
bulk_events <- function(mock) {
  out <- list()
  for (r in mock$requests("/bulk")) {
    if (nzchar(r$body)) out <- c(out, jsonlite::fromJSON(r$body, simplifyVector = FALSE))
  }
  out
}
