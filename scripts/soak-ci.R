#!/usr/bin/env Rscript
# CI-safe shortened soak: memory + reconnect under local webfakes FDv2 poll.
# For a full 24h soak see scripts/soak-24h.R (manual / long runner).

suppressPackageStartupMessages({
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(launchdarklyServer)
  }
  library(webfakes)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

duration_s <- as.numeric(Sys.getenv("LD_SOAK_SECONDS", unset = "30"))
poll_ms <- as.numeric(Sys.getenv("LD_SOAK_POLL_MS", unset = "200"))

app <- webfakes::new_app()
version <- 1L
app$get("/sdk/poll", function(req, res) {
  version <<- version + 1L
  body <- list(
    events = list(
      list(
        event = "server-intent",
        data = list(payloads = list(list(
          id = "p", target = 1, intentCode = "xfer-full", reason = "soak"
        )))
      ),
      list(
        event = "put-object",
        data = list(
          kind = "flag",
          key = "soak-flag",
          version = version,
          object = list(
            key = "soak-flag",
            on = TRUE,
            version = version,
            variations = list(FALSE, TRUE),
            fallthrough = list(variation = 1L),
            salt = "soak"
          )
        )
      ),
      list(
        event = "payload-transferred",
        data = list(state = sprintf("s-%s", version), version = version)
      )
    )
  )
  res$set_header("Content-Type", "application/json")
  res$send(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA))
})

server <- webfakes::new_app_process(app)
on.exit(server$stop(), add = TRUE)

cfg <- ld_config(
  "soak-key",
  base_uri = server$url(),
  stream = FALSE,
  send_events = FALSE,
  poll_interval = poll_ms / 1000,
  start_wait = 3,
  read_timeout = 5
)

mem0 <- sum(gc()[, 2])
client <- ld_client(cfg, start_wait = 3)
on.exit(client$close(), add = TRUE)

ctx <- ld_context("soak-user")
deadline <- Sys.time() + duration_s
evals <- 0L
reconnects <- 0L
last_init <- client$is_initialized()

while (Sys.time() < deadline) {
  val <- client$variation("soak-flag", ctx, FALSE)
  evals <- evals + 1L
  init <- client$is_initialized()
  if (!identical(init, last_init)) {
    reconnects <- reconnects + 1L
    last_init <- init
  }
  # Force a brief pause so the worker can poll / "reconnect"
  Sys.sleep(poll_ms / 1000)
}

mem1 <- sum(gc()[, 2])
was_init <- isTRUE(last_init)
client$close()

cat(sprintf(
  "soak-ci: duration=%.0fs evals=%d reconnect_flips=%d mem_Mb_before=%.1f mem_Mb_after=%.1f delta=%.1f last_initialized=%s\n",
  duration_s, evals, reconnects, mem0, mem1, mem1 - mem0, was_init
))

# Soft guard: R heap should not balloon unboundedly in a short soak
if ((mem1 - mem0) > 200) {
  message("WARN: memory delta > 200 Mb during short soak — investigate before 24h run")
  quit(status = 1)
}
cat("soak-ci: PASS\n")
