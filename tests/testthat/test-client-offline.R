test_that("offline client initializes immediately and evaluates store data", {
  cfg <- ld_config("sdk-key", offline = TRUE, start_wait = 0)
  client <- ld_client(cfg)
  on.exit(client$close(), add = TRUE)
  expect_true(client$is_initialized())
  expect_true(client$is_offline())

  ctx <- ld_context("user-1")
  expect_equal(client$variation("missing", ctx, "d"), "d")

  client$.update_store_for_tests(flags = list(
    "flag-a" = list(
      key = "flag-a", version = 1L, on = TRUE,
      variations = list("off", "on"),
      fallthrough = list(variation = 1L)
    ),
    "flag-off" = list(
      key = "flag-off", version = 1L, on = FALSE,
      variations = list("a", "b"), offVariation = 0L
    )
  ))

  expect_equal(client$variation("flag-a", ctx, "d"), "on")
  detail <- client$variation_detail("flag-off", ctx, "d")
  expect_equal(detail$value, "a")
  expect_equal(detail$reason$kind, "OFF")

  state <- client$all_flags_state(ctx, with_reasons = TRUE)
  expect_true(state$valid())
  expect_equal(state$get_flag_value("flag-a"), "on")

  h <- client$secure_mode_hash(ctx)
  expect_true(is.character(h) && nchar(h) == 64)
})

test_that("webfakes polling initializes client store", {
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app()
  app$get("/sdk/poll", function(req, res) {
    body <- list(events = list(
      list(event = "server-intent", data = list(
        payloads = list(list(intentCode = "xfer-full", id = "p", target = 1, reason = "initial"))
      )),
      list(event = "put-object", data = list(
        kind = "flag", key = "poll-flag", version = 1,
        object = list(
          key = "poll-flag", on = TRUE, version = 1,
          variations = list(FALSE, TRUE),
          fallthrough = list(variation = 1)
        )
      )),
      list(event = "payload-transferred", data = list(state = "basis-1", version = 1))
    ))
    res$set_header("Content-Type", "application/json")
    res$send(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)

  # Wait until the mock is actually accepting connections (avoids startup races
  # under R CMD check where the worker can poll before httpuv is ready).
  ready <- FALSE
  deadline <- Sys.time() + 5
  while (Sys.time() < deadline) {
    ok <- tryCatch({
      resp <- httr2::req_perform(
        httr2::request(paste0(sub("/$", "", server$url()), "/sdk/poll"))
      )
      httr2::resp_status(resp) == 200
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      ready <- TRUE
      break
    }
    Sys.sleep(0.05)
  }
  skip_if_not(ready, "webfakes poll mock did not become ready")

  cfg <- ld_config(
    "test-sdk-key",
    base_uri = server$url(),
    stream = FALSE,
    poll_interval = 60,
    start_wait = 10,
    send_events = FALSE
  )
  client <- ld_client(cfg)
  on.exit(client$close(), add = TRUE)

  expect_true(client$is_initialized())
  ctx <- ld_context("u")
  expect_equal(client$variation("poll-flag", ctx, FALSE), TRUE)
})
