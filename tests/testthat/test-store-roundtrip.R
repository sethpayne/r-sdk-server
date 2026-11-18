test_that("store snapshots round-trip through worker IPC JSON", {
  s <- FeatureStore$new()
  s$apply_changeset(list(
    intent = "xfer-full",
    puts = list(
      list(kind = "flag", key = "f", version = 3L, object = simple_flag("f", version = 3L,
           variations = list(1L, 2.5, "s", list(a = 1), list(1, 2), NULL))),
      list(kind = "segment", key = "seg", version = 1L,
           object = list(key = "seg", version = 1L, included = list("a")))
    ),
    selector = list(state = "st", version = 4L)
  ))
  json <- jsonlite::toJSON(list(payload = s$to_snapshot()), auto_unbox = TRUE, null = "null", digits = NA)
  back <- from_json(json)$payload
  s2 <- FeatureStore$new()
  s2$replace_all(back$flags, back$segments, back$selector)
  expect_equal(s2$get_flag("f"), s$get_flag("f"))
  expect_equal(s2$get_segment("seg"), s$get_segment("seg"))
  expect_equal(s2$selector(), list(state = "st", version = 4L))
})

test_that("deletes without a version tombstone the item", {
  s <- FeatureStore$new()
  s$upsert("flag", "f", 2L, simple_flag("f", version = 2L))
  expect_true(s$delete("flag", "f", NULL))
  expect_true(s$get_flag("f")$deleted %||% is.null(s$get_flag("f")))
  s$upsert("flag", "g", 2L, simple_flag("g", version = 2L))
  expect_true(s$delete("flag", "g", NA))
  expect_false(s$delete("flag", "", 1L))
  expect_false(s$delete("flag", NA_character_, 1L))
})

test_that("stale upserts and deletes are ignored", {
  s <- FeatureStore$new()
  s$upsert("flag", "f", 5L, simple_flag("f", version = 5L))
  expect_false(s$upsert("flag", "f", 4L, simple_flag("f", version = 4L)))
  expect_equal(s$get_flag("f")$version, 5L)
  expect_false(s$delete("flag", "f", 4L))
  expect_equal(s$get_flag("f")$version, 5L)
  expect_true(s$upsert("flag", "f", c(1L, 2L), simple_flag("f", version = 6L)))
})

test_that("invalid keys never reach the store", {
  s <- FeatureStore$new()
  for (k in list("", NA_character_, NULL, c("a", "b"), 1)) {
    expect_false(s$upsert("flag", k, 1L, list(on = TRUE)))
    expect_null(s$get_flag(k))
  }
  s$replace_all(flags = list(ok = simple_flag("ok")))
  expect_equal(names(s$all_flags()), "ok")
})
