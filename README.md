# launchdarklyServer

**Unofficial** LaunchDarkly server-side SDK for R (FDv2).

> Not affiliated with or endorsed by LaunchDarkly, Inc. Official SDK name and user-agent identity remain pending LaunchDarkly assignment. Do not use LaunchDarkly logos or trademarks in a way that implies endorsement.

## Install

```r
# remotes::install_github("sethpayne/r-sdk-server")
```

## Quick start

```r
library(launchdarklyServer)

cfg <- ld_config(
  Sys.getenv("LAUNCHDARKLY_SDK_KEY"),
  start_wait = 5,
  stream = TRUE
)
client <- ld_client(cfg)
on.exit(client$close(), add = TRUE)

ctx <- ld_context("user-123", email = "user@example.com")
value <- client$variation("my-flag", ctx, FALSE)
```

Offline / tests:

```r
client <- ld_client(ld_config("any", offline = TRUE))
td <- ld_test_data()
td$flag("my-flag", on = TRUE, variations = list(FALSE, TRUE), fallthrough = list(variation = 1L))
td$apply_to_client(client)
```

## Process model

A **callr/processx background worker** owns FDv2 sync and the event buffer. The parent process evaluates against a local store snapshot. See [DESIGN.md](DESIGN.md).

## Unit and integration tests

```bash
NOT_CRAN=true Rscript -e 'devtools::test()'   # full suite, incl. mock-service tests
Rscript scripts/coverage-gate.R               # ≥90% on evaluator/context/data system/events
```

Tests that start a worker against the webfakes mock service (`tests/testthat/helper-mock-ld.R`) are skipped on CRAN; fork tests are skipped on Windows.

## Contract tests

```bash
Rscript contract-tests/service.R 8000
# Point sdk-test-harness v3 at http://127.0.0.1:8000
```

Suppressions (with justifications) live in `contract-tests/suppressions.txt`.

## Status

See [FEATURES.md](FEATURES.md) and [CHANGELOG.md](CHANGELOG.md). License: Apache-2.0.
