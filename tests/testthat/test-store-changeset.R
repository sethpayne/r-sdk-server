test_that("changeset put/delete and full transfer", {
  store <- FeatureStore$new()
  cs <- list(
    intent = "xfer-full",
    puts = list(
      list(kind = "flag", key = "f1", version = 1L, object = list(
        key = "f1", on = TRUE, variations = list(FALSE, TRUE), fallthrough = list(variation = 1L)
      )),
      list(kind = "segment", key = "s1", version = 1L, object = list(key = "s1", included = list("a")))
    ),
    deletes = list(),
    selector = list(state = "abc", version = 3L)
  )
  store$apply_changeset(cs)
  expect_true(store$is_initialized())
  expect_equal(store$get_flag("f1")$key, "f1")
  expect_equal(store$get_segment("s1")$key, "s1")
  expect_equal(store$selector()$state, "abc")

  store$apply_changeset(list(
    intent = "xfer-changes",
    puts = list(),
    deletes = list(list(kind = "flag", key = "f1", version = 2L)),
    selector = list(state = "def", version = 4L)
  ))
  expect_true(isTRUE(store$get_flag("f1")$deleted))
})

test_that("polling payload parser handles xfer-full events", {
  payload <- list(events = list(
    list(event = "server-intent", data = list(payload = list(code = "xfer-full"))),
    list(event = "put-object", data = list(
      kind = "flag", key = "a", version = 1, object = list(key = "a", on = FALSE, variations = list("x"), offVariation = 0)
    )),
    list(event = "payload-transferred", data = list(state = "s1", version = 9))
  ))
  cs <- fdv2_polling_payload_to_changeset(payload)
  expect_null(cs$error)
  expect_equal(cs$selector$state, "s1")
  expect_equal(length(cs$puts), 1)
})

test_that("polling payload transfer-none", {
  payload <- list(events = list(
    list(event = "server-intent", data = list(payload = list(code = "xfer-none")))
  ))
  cs <- fdv2_polling_payload_to_changeset(payload)
  expect_true(isTRUE(cs$no_changes))
})
