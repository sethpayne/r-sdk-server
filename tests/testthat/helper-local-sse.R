# Local keep-alive SSE server for offline streaming coverage (no live LD network).

.ld_start_local_sse <- function(handler, host = "127.0.0.1") {
  # handler(path) -> list(status, headers, body_chunks, hold_seconds)
  # body_chunks: character vector of SSE chunks written then flushed
  port <- httpuv::randomPort()
  script <- tempfile(fileext = ".py")
  # Encode handler behavior as a simple mode string for the Python process
  writeLines(c(
    "import http.server, time, sys, json",
    "HOST, PORT, MODE = sys.argv[1], int(sys.argv[2]), sys.argv[3]",
    "class H(http.server.BaseHTTPRequestHandler):",
    "  def log_message(self, *a): pass",
    "  def do_GET(self):",
    "    path = self.path.split('?',1)[0]",
    "    if MODE == 'ok':",
    "      self.send_response(200)",
    "      self.send_header('Content-Type','text/event-stream')",
    "      self.send_header('Cache-Control','no-cache')",
    "      self.send_header('X-LD-EnvID','env1')",
    "      self.end_headers()",
    "      chunks = [",
    "        'event: server-intent\\ndata: {\"payloads\":[{\"id\":\"p\",\"target\":1,\"intentCode\":\"xfer-full\",\"reason\":\"initial\"}]}\\n\\n',",
    "        'event: put-object\\ndata: {\"kind\":\"flag\",\"key\":\"f\",\"version\":1,\"object\":{\"key\":\"f\",\"version\":1,\"on\":true,\"variations\":[false,true],\"fallthrough\":{\"variation\":1}}}\\n\\n',",
    "        'event: payload-transferred\\ndata: {\"state\":\"initial\",\"version\":1}\\n\\n'",
    "      ]",
    "      for c in chunks:",
    "        self.wfile.write(c.encode()); self.wfile.flush()",
    "      time.sleep(30)",
    "    elif MODE == 'multi':",
    "      self.send_response(200)",
    "      self.send_header('Content-Type','text/event-stream')",
    "      self.end_headers()",
    "      self.wfile.write(b'event: server-intent\\ndata: {\"payloads\":[{\"intentCode\":\"xfer-full\"}]}\\n\\n')",
    "      self.wfile.write(b'event: payload-transferred\\ndata: {\"state\":\"a\",\"version\":1}\\n\\n')",
    "      self.wfile.flush(); time.sleep(0.2)",
    "      self.wfile.write(b'event: put-object\\ndata: {\"kind\":\"flag\",\"key\":\"g\",\"version\":2,\"object\":{\"key\":\"g\",\"version\":2,\"on\":true,\"variations\":[0,1],\"fallthrough\":{\"variation\":0}}}\\n\\n')",
    "      self.wfile.write(b'event: payload-transferred\\ndata: {\"state\":\"b\",\"version\":2}\\n\\n')",
    "      self.wfile.flush(); time.sleep(30)",
    "    elif MODE == 'goodbye':",
    "      self.send_response(200)",
    "      self.send_header('Content-Type','text/event-stream')",
    "      self.end_headers()",
    "      self.wfile.write(b'event: goodbye\\ndata: {\"reason\":\"bye\"}\\n\\n'); self.wfile.flush(); time.sleep(5)",
    "    elif MODE == 'error_then_ok':",
    "      self.send_response(200)",
    "      self.send_header('Content-Type','text/event-stream')",
    "      self.end_headers()",
    "      self.wfile.write(b'event: put-object\\ndata: {\"kind\":\"flag\",\"key\":\"x\",\"version\":1,\"object\":{\"key\":\"x\"}}\\n\\n')",
    "      self.wfile.write(b'event: error\\ndata: {\"payloadId\":\"p\",\"reason\":\"r\"}\\n\\n')",
    "      self.wfile.write(b'event: server-intent\\ndata: {\"payloads\":[{\"intentCode\":\"xfer-changes\"}]}\\n\\n')",
    "      self.wfile.write(b'event: put-object\\ndata: {\"kind\":\"flag\",\"key\":\"f\",\"version\":2,\"object\":{\"key\":\"f\",\"version\":2,\"on\":true,\"variations\":[false,true],\"fallthrough\":{\"variation\":1}}}\\n\\n')",
    "      self.wfile.write(b'event: payload-transferred\\ndata: {\"state\":\"ok\",\"version\":2}\\n\\n')",
    "      self.wfile.flush(); time.sleep(30)",
    "    elif MODE == 'badjson':",
    "      self.send_response(200)",
    "      self.send_header('Content-Type','text/event-stream')",
    "      self.end_headers()",
    "      self.wfile.write(b'event: server-intent\\ndata: {not-json\\n\\n'); self.wfile.flush(); time.sleep(5)",
    "    elif MODE == '401':",
    "      self.send_response(401); self.end_headers()",
    "    elif MODE == '403fb':",
    "      self.send_response(403)",
    "      self.send_header('X-LD-FD-Fallback','true')",
    "      self.end_headers()",
    "    else:",
    "      self.send_response(404); self.end_headers()",
    "http.server.HTTPServer((HOST, PORT), H).serve_forever()"
  ), script)
  mode <- handler
  proc <- processx::process$new(
    "python3", c(script, host, as.character(port), mode),
    stdout = "|", stderr = "|"
  )
  Sys.sleep(0.25)
  list(
    url = sprintf("http://%s:%s", host, port),
    port = port,
    process = proc,
    stop = function() {
      if (proc$is_alive()) proc$kill(close_connections = TRUE)
      unlink(script)
    }
  )
}
