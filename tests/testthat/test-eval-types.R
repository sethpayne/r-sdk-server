# Evaluation with flags parsed from JSON (as the worker delivers them) against
# contexts built the way R users write them.

eval_json_flag <- function(flag_json, context, default = "default", segments = list()) {
  flag <- from_json(flag_json)
  ld_evaluate_flag(flag, context, default,
                   get_flag = function(k) NULL,
                   get_segment = function(k) segments[[k]])
}

rule_flag_json <- function(attribute, op, values_json) {
  sprintf('{"key":"f","version":1,"on":true,"salt":"s","variations":["no","yes"],
    "fallthrough":{"variation":0},"offVariation":0,
    "rules":[{"id":"r","variation":1,"clauses":[
      {"attribute":"%s","op":"%s","values":%s,"negate":false}]}]}', attribute, op, values_json)
}

test_that("`in` matches numbers regardless of integer/double representation (D1)", {
  contexts <- list(
    double = ld_context("u", age = 30),
    integer = ld_context("u", age = 30L),
    json_double = context_from_json('{"kind":"user","key":"u","age":30.0}'),
    json_int = context_from_json('{"kind":"user","key":"u","age":30}')
  )
  for (clause_values in c("[30]", "[30.0]", "[1, 30]")) {
    flag <- rule_flag_json("age", "in", clause_values)
    for (nm in names(contexts)) {
      expect_equal(eval_json_flag(flag, contexts[[nm]])$value, "yes",
                   label = sprintf("%s context vs values %s", nm, clause_values))
    }
  }
  expect_equal(eval_json_flag(rule_flag_json("age", "in", "[31]"), contexts$double)$value, "no")
})

test_that("`in` keeps types distinct: strings, numbers, booleans do not cross-match", {
  expect_equal(eval_json_flag(rule_flag_json("v", "in", '["30"]'), ld_context("u", v = 30))$value, "no")
  expect_equal(eval_json_flag(rule_flag_json("v", "in", "[30]"), ld_context("u", v = "30"))$value, "no")
  expect_equal(eval_json_flag(rule_flag_json("v", "in", "[1]"), ld_context("u", v = TRUE))$value, "no")
  expect_equal(eval_json_flag(rule_flag_json("v", "in", "[true]"), ld_context("u", v = TRUE))$value, "yes")
  expect_equal(eval_json_flag(rule_flag_json("v", "in", '["a"]'), ld_context("u", v = NA_character_))$value, "no")
})

test_that("numeric comparison operators accept JSON integers against R doubles", {
  flag <- rule_flag_json("age", "greaterThanOrEqual", "[18]")
  expect_equal(eval_json_flag(flag, ld_context("u", age = 18))$value, "yes")
  expect_equal(eval_json_flag(flag, ld_context("u", age = 17.5))$value, "no")
})

test_that("atomic vectors and factors act as multi-valued attributes (D2)", {
  flag <- rule_flag_json("groups", "in", '["beta"]')
  expect_equal(eval_json_flag(flag, ld_context("u", groups = c("alpha", "beta")))$value, "yes")
  expect_equal(eval_json_flag(flag, ld_context("u", groups = list("alpha", "beta")))$value, "yes")
  expect_equal(eval_json_flag(flag, ld_context("u", groups = factor(c("alpha", "beta"))))$value, "yes")
  expect_equal(eval_json_flag(flag, ld_context("u", groups = "beta"))$value, "yes")
  expect_equal(eval_json_flag(flag, ld_context("u", groups = character()))$value, "no")
  expect_equal(eval_json_flag(flag, ld_context("u", groups = c("alpha", "gamma")))$value, "no")

  num_flag <- rule_flag_json("scores", "in", "[3]")
  expect_equal(eval_json_flag(num_flag, ld_context("u", scores = c(1, 2, 3)))$value, "yes")
})

test_that("negated clauses over vectors negate the whole match", {
  flag <- from_json(rule_flag_json("groups", "in", '["beta"]'))
  flag$rules[[1]]$clauses[[1]]$negate <- TRUE
  d <- ld_evaluate_flag(flag, ld_context("u", groups = c("alpha", "beta")), "d",
                        function(k) NULL, function(k) NULL)
  expect_equal(d$value, "no")
})

test_that("bucketing matches LaunchDarkly reference vectors", {
  b <- function(key) .ld_bucket_context(NULL, context_create(key), NULL, "hashKey", "saltyA", NULL)
  expect_equal(b("userKeyA"), 0.42157587, tolerance = 1e-7)
  expect_equal(b("userKeyB"), 0.6708485, tolerance = 1e-7)
  expect_equal(b("userKeyC"), 0.10343106, tolerance = 1e-7)
  # integer-valued numeric bucketBy attributes stringify without a fraction
  ctx <- ld_context("x", n = 33333)
  expect_equal(
    .ld_bucket_context(NULL, ctx, NULL, "hashKey", "saltyA", "n"),
    .ld_bucket_context(NULL, context_create("33333"), NULL, "hashKey", "saltyA", NULL)
  )
  expect_equal(.ld_bucket_context(NULL, ld_context("x", n = 1.5), NULL, "hashKey", "saltyA", "n"), 0)
})

test_that("bucketing hashes UTF-8 bytes independent of locale", {
  key <- enc2utf8("\u00fc-user")
  # python: int(sha1(b"hashKey.saltyA.\xc3\xbc-user").hexdigest()[:15], 16) / 0xFFFFFFFFFFFFFFF
  expected <- 0.4317525097988345
  expect_equal(.ld_bucket_context(NULL, context_create(key), NULL, "hashKey", "saltyA", NULL),
               expected, tolerance = 1e-12)
  withr::with_locale(c(LC_CTYPE = "C"), {
    expect_equal(.ld_bucket_context(NULL, context_create(key), NULL, "hashKey", "saltyA", NULL),
                 expected, tolerance = 1e-12)
  })
})

test_that("rollouts pick the variation for the bucket", {
  flag <- from_json('{"key":"hashKey","salt":"saltyA","version":1,"on":true,
    "variations":["a","b","c"],
    "fallthrough":{"rollout":{"variations":[
      {"variation":0,"weight":40000},{"variation":1,"weight":30000},{"variation":2,"weight":30000}]}}}')
  ev <- function(k) ld_evaluate_flag(flag, context_create(k), "d", function(x) NULL, function(x) NULL)$value
  expect_equal(ev("userKeyA"), "b") # 0.4216 -> second bucket
  expect_equal(ev("userKeyB"), "b") # 0.6708
  expect_equal(ev("userKeyC"), "a") # 0.1034
})

test_that("experiment rollouts mark inExperiment unless untracked", {
  flag <- from_json('{"key":"hashKey","salt":"saltyA","version":1,"on":true,
    "variations":["a","b"],
    "fallthrough":{"rollout":{"kind":"experiment","variations":[
      {"variation":0,"weight":50000},{"variation":1,"weight":50000,"untracked":true}]}}}')
  ev <- function(k) ld_evaluate_flag(flag, context_create(k), "d", function(x) NULL, function(x) NULL)
  a <- ev("userKeyA") # 0.42 -> tracked variation 0
  expect_equal(a$value, "a")
  expect_true(isTRUE(a$reason$inExperiment))
  b <- ev("userKeyB") # 0.67 -> untracked variation 1
  expect_equal(b$value, "b")
  expect_null(b$reason$inExperiment)
})

test_that("secure_mode_hash matches the LaunchDarkly reference vector", {
  client <- local_ld_client("secret", offline = TRUE)
  expect_equal(client$secure_mode_hash(context_create("Message")),
               "aa747c502a898200f9e4fa21bac68136f886a0e27aec70ba06daf2e2a5cb5597")
  expect_equal(client$secure_mode_hash(context_create("")), "")
})

test_that("date operators are independent of the session time zone", {
  flag <- rule_flag_json("t", "before", '["2017-12-06T00:00:00.000-07:00"]')
  ctx_before <- ld_context("u", t = "2017-12-06T06:59:59.000Z")
  ctx_after <- ld_context("u", t = "2017-12-06T07:00:01.000Z")
  for (tz in c("UTC", "America/New_York", "Asia/Tokyo")) {
    withr::with_timezone(tz, {
      expect_equal(eval_json_flag(flag, ctx_before)$value, "yes", label = tz)
      expect_equal(eval_json_flag(flag, ctx_after)$value, "no", label = tz)
    })
  }
  ms_flag <- rule_flag_json("t", "after", "[1512543600000]")
  expect_equal(eval_json_flag(ms_flag, ld_context("u", t = 1512543600001))$value, "yes")
})

test_that("malformed variations report MALFORMED_FLAG with the default value (D9)", {
  cases <- list(
    target_without_variation = '{"key":"f","version":1,"on":true,"variations":["a"],
      "fallthrough":{"variation":0},"targets":[{"values":["u"]}]}',
    variation_too_high = '{"key":"f","version":1,"on":true,"variations":["a"],
      "fallthrough":{"variation":5}}',
    negative_off_variation = '{"key":"f","version":1,"on":false,"variations":["a"],"offVariation":-1}',
    fallthrough_missing = '{"key":"f","version":1,"on":true,"variations":["a"]}',
    rule_variation_string = '{"key":"f","version":1,"on":true,"variations":["a"],
      "fallthrough":{"variation":0},
      "rules":[{"variation":"x","clauses":[{"attribute":"key","op":"in","values":["u"]}]}]}'
  )
  for (nm in names(cases)) {
    d <- eval_json_flag(cases[[nm]], context_create("u"), "dflt")
    expect_equal(d$value, "dflt", label = nm)
    expect_null(d$variation_index, label = nm)
    expect_equal(d$reason$errorKind, "MALFORMED_FLAG", label = nm)
  }
})
