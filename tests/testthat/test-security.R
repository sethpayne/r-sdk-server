secret <- "sdk-SUPERSECRET-0123456789"

test_that("printing or formatting a config never reveals the SDK key (D6)", {
  cfg <- ld_config(secret, base_uri = "http://127.0.0.1:1")
  out <- c(capture.output(print(cfg)), format(cfg), capture.output(str(cfg)), capture.output(cfg))
  expect_false(any(grepl(secret, out, fixed = TRUE)))
  expect_true(any(grepl("sdk_key: <redacted>", out, fixed = TRUE)))
  expect_true(any(grepl("base_uri: http://127.0.0.1:1", out, fixed = TRUE)))
})

test_that("failed HTTP calls do not put the SDK key in errors", {
  cfg <- list(sdk_key = secret, base_uri = "http://127.0.0.1:1", stream_uri = "http://127.0.0.1:1",
              events_uri = "http://127.0.0.1:1", send_events = TRUE, read_timeout = 1,
              connect_timeout = 1)
  texts <- c(
    fdv2_poll_once(cfg)$error,
    ld_post_events(cfg, list(list(kind = "identify")), retry_delay = 0)$error,
    fdv2_stream_once(cfg, until_first_payload = TRUE)$error
  )
  expect_length(texts, 3)
  expect_false(any(grepl(secret, texts, fixed = TRUE)))
})

test_that("worker log and fatal messages are scrubbed of the SDK key", {
  client <- local_ld_client(secret, offline = TRUE)
  handle <- client$.__enclos_env__$private$handle_worker_message
  msgs <- capture_messages(handle(list(type = "Log", payload = list(level = "warn",
                                                                    message = paste("bad key", secret)))))
  expect_false(any(grepl(secret, msgs, fixed = TRUE)))
  expect_match(msgs, "<redacted>", fixed = TRUE)
  w <- capture_warnings(handle(list(type = "Fatal", payload = list(message = secret))))
  expect_false(any(grepl(secret, w, fixed = TRUE)))
})

test_that("worker stderr and logs never contain the SDK key", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", list(status = 401L, body = "unauthorized"))
  out <- capture_messages(capture_warnings({
    client <- local_polling_client(mock, secret, start_wait = 3)
    client$is_initialized()
    Sys.sleep(0.5)
    client$is_initialized()
  }))
  expect_false(any(grepl(secret, out, fixed = TRUE)))
})
