fmt <- function(ctx, ..., redact_anonymous = FALSE) {
  .ld_format_event_context(context_to_list(ctx), ..., redact_anonymous = redact_anonymous)
}

test_that("top-level private attributes are removed and reported", {
  out <- fmt(ld_context("u", email = "e@x", plan = "gold"), private_attrs = "email")
  expect_null(out$email)
  expect_equal(out$plan, "gold")
  expect_equal(out[["_meta"]]$redactedAttributes, list("email"))
})

test_that("nested attribute paths are redacted, siblings kept (D3)", {
  ctx <- ld_context("u", address = list(street = "1 Main", city = "Oslo", geo = list(lat = 1, lng = 2)))
  out <- fmt(ctx, private_attrs = c("/address/street", "/address/geo/lat"))
  expect_null(out$address$street)
  expect_equal(out$address$city, "Oslo")
  expect_null(out$address$geo$lat)
  expect_equal(out$address$geo$lng, 2)
  expect_setequal(unlist(out[["_meta"]]$redactedAttributes), c("/address/street", "/address/geo/lat"))
})

test_that("references that match nothing are not reported", {
  out <- fmt(ld_context("u", a = 1), private_attrs = c("/missing", "/a/b", "missing2"))
  expect_equal(out$a, 1)
  expect_null(out[["_meta"]])
})

test_that("escaped references (~1 and ~0) address literal names", {
  ctx <- ld_context("u", `a/b` = 1, `c~d` = 2, keep = 3)
  out <- fmt(ctx, private_attrs = c("/a~1b", "/c~0d"))
  expect_null(out[["a/b"]])
  expect_null(out[["c~d"]])
  expect_equal(out$keep, 3)
})

test_that("context-level private attributes apply and _meta.privateAttributes is not sent", {
  ctx <- ld_context("u", email = "e", phone = "p", address = list(zip = "1"),
                    private = c("email", "/address/zip"))
  out <- fmt(ctx)
  expect_null(out$email)
  expect_equal(out$phone, "p")
  expect_null(out$address$zip)
  expect_null(out[["_meta"]]$privateAttributes)
  expect_setequal(unlist(out[["_meta"]]$redactedAttributes), c("email", "/address/zip"))
})

test_that("key, kind and anonymous are never redacted", {
  ctx <- ld_context("u", anonymous = TRUE, x = 1)
  out <- fmt(ctx, private_attrs = c("key", "/kind", "anonymous"))
  expect_equal(out$key, "u")
  expect_equal(out$kind, "user")
  expect_true(out$anonymous)
  expect_equal(out$x, 1)
})

test_that("all_attributes_private redacts name and custom attributes", {
  out <- fmt(ld_context("u", name = "Ada", email = "e"), all_private = TRUE)
  expect_equal(names(out), c("key", "kind", "_meta"))
  expect_setequal(unlist(out[["_meta"]]$redactedAttributes), c("name", "email"))
})

test_that("anonymous contexts are fully redacted only when requested", {
  ctx <- ld_context("u", anonymous = TRUE, email = "e")
  expect_equal(fmt(ctx)$email, "e")
  red <- fmt(ctx, redact_anonymous = TRUE)
  expect_null(red$email)
  expect_true(red$anonymous)
})

test_that("multi-contexts redact each kind without inner kind fields", {
  ctx <- context_create_multi(
    ld_context("u", email = "e", private = "email"),
    ld_context("o", kind = "org", name = "Acme", tier = 1)
  )
  out <- fmt(ctx, private_attrs = "tier")
  expect_equal(out$kind, "multi")
  expect_null(out$user$kind)
  expect_null(out$user$email)
  expect_equal(out$org$name, "Acme")
  expect_null(out$org$tier)
})

test_that("redacted event JSON is stable", {
  buf <- EventBuffer$new(private_attrs = c("/address/street"))
  ctx <- ld_context("u", name = "Ada", email = "e", address = list(street = "s", city = "c"),
                    private = "email")
  buf$add_identify(context_to_list(ctx), creation_date = 1000)
  buf$add_custom("evt", context_to_list(ctx), data = list(n = 1), metric_value = 2.5,
                 creation_date = 1001)
  expect_snapshot(cat(jsonlite::toJSON(buf$drain(), auto_unbox = TRUE, pretty = TRUE)))
})
