# Local SSE server for offline streaming coverage (no live LD network).
# Runs in a webfakes subprocess so curl clients in this R process cannot deadlock
# the httpuv event loop (and so macOS CI does not depend on a Python sidecar).

.ld_start_local_sse <- function(handler, host = "127.0.0.1") {
  skip_if_not_installed("webfakes")
  mode <- handler
  app <- webfakes::new_app()

  app$get("/sdk/stream", function(req, res) {
    if (identical(mode, "ok")) {
      res$set_header("Content-Type", "text/event-stream")
      res$set_header("Cache-Control", "no-cache")
      res$set_header("X-LD-EnvID", "env1")
      res$send(paste0(
        'event: server-intent\ndata: {"payloads":[{"id":"p","target":1,"intentCode":"xfer-full","reason":"initial"}]}\n\n',
        'event: put-object\ndata: {"kind":"flag","key":"f","version":1,"object":{"key":"f","version":1,"on":true,"variations":[false,true],"fallthrough":{"variation":1}}}\n\n',
        'event: payload-transferred\ndata: {"state":"initial","version":1}\n\n'
      ))
      return()
    }
    if (identical(mode, "multi")) {
      res$set_header("Content-Type", "text/event-stream")
      res$send(paste0(
        'event: server-intent\ndata: {"payloads":[{"intentCode":"xfer-full"}]}\n\n',
        'event: payload-transferred\ndata: {"state":"a","version":1}\n\n',
        'event: put-object\ndata: {"kind":"flag","key":"g","version":2,"object":{"key":"g","version":2,"on":true,"variations":[0,1],"fallthrough":{"variation":0}}}\n\n',
        'event: payload-transferred\ndata: {"state":"b","version":2}\n\n'
      ))
      return()
    }
    if (identical(mode, "goodbye")) {
      res$set_header("Content-Type", "text/event-stream")
      res$send('event: goodbye\ndata: {"reason":"bye"}\n\n')
      return()
    }
    if (identical(mode, "error_then_ok")) {
      res$set_header("Content-Type", "text/event-stream")
      res$send(paste0(
        'event: put-object\ndata: {"kind":"flag","key":"x","version":1,"object":{"key":"x"}}\n\n',
        'event: error\ndata: {"payloadId":"p","reason":"r"}\n\n',
        'event: server-intent\ndata: {"payloads":[{"intentCode":"xfer-changes"}]}\n\n',
        'event: put-object\ndata: {"kind":"flag","key":"f","version":2,"object":{"key":"f","version":2,"on":true,"variations":[false,true],"fallthrough":{"variation":1}}}\n\n',
        'event: payload-transferred\ndata: {"state":"ok","version":2}\n\n'
      ))
      return()
    }
    if (identical(mode, "badjson")) {
      res$set_header("Content-Type", "text/event-stream")
      res$send("event: server-intent\ndata: {not-json\n\n")
      return()
    }
    if (identical(mode, "401")) {
      res$set_status(401L)$send("")
      return()
    }
    if (identical(mode, "403fb")) {
      res$set_status(403L)
      res$set_header("X-LD-FD-Fallback", "true")
      res$send("")
      return()
    }
    res$set_status(404L)$send("")
  })

  # Also accept bare "/" so accidental path differences still hit the handler.
  app$get("/", function(req, res) {
    res$redirect("/sdk/stream")
  })

  proc <- webfakes::new_app_process(app, opts = webfakes::server_opts(port = NULL))
  list(
    url = sub("/$", "", proc$url()),
    port = NA_integer_,
    process = proc,
    stop = function() {
      try(proc$stop(), silent = TRUE)
    }
  )
}
