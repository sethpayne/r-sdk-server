# Changelog

All notable changes to this unofficial project are documented here.

## [0.0.0.9000] — unreleased

### Added
- Milestone 1: DESIGN / FEATURES / NOTES, package skeleton, identity constants
- Milestone 2: Evaluation `ld_context` model + unit tests
- Milestone 3: httpuv sdk-test-harness v3 contract service (contextBuild/Convert)
- Milestone 4: FDv2 polling, FeatureStore, offline client, start wait, process worker
- Milestone 5: Flag evaluator (rules, targets, segments, rollouts, prereqs)
- Milestone 6: Analytics event buffer + flush (`/bulk`, schema 4)
- Milestone 7: FDv2 streaming SSE sync with poll fallback/recovery; fdv1Fallback fail-safe
- Milestone 8: Hooks, flag change listeners, TestData source
- Milestone 9: Hardening tests (fork/postfork, shutdown, micro-benchmark), SECURITY.md
- Milestone 10: README, vignettes, pkgdown config, CHANGELOG

### Fixed (test-suite review)
- Evaluation: integer-valued JSON numbers (`30` parsed as integer) now match numeric clauses; `in` compares numbers numerically; R vectors and factors are treated as arrays / strings; malformed variation indices yield `MALFORMED_FLAG`
- Contexts: keys must be one non-NA string; `ld_context()` options follow `...` so custom attributes such as `n = 1` no longer bind to `name`; `context_from_json()` rejects non-object JSON
- Events: `variation()` now emits index, feature, debug and summary events; nested private-attribute references are redacted and reported in `_meta.redactedAttributes`; anonymous contexts are redacted in feature/custom events; `/bulk` posts carry `X-LaunchDarkly-Payload-ID`, retry once on recoverable errors and stop after 401/403; the buffer drops new events when full
- Worker: a malformed poll or stream payload no longer ends the worker loop; polling 401/403 turns the data source off; a dead worker is restarted with backoff and the last snapshot keeps serving
- Client: public methods never throw; `track()` / `identify()` / `flush()` are no-ops in a forked child; hooks cannot replace the evaluation context; value listeners record their starting value; new `data_source_status()`
- IPC: writes the stdin pipe cannot take are buffered (capped at 16 MB) instead of being lost
- Config: `print()` / `format()` mask the SDK key; unknown or mistyped options warn and keep defaults

### Tests
- Scriptable webfakes LaunchDarkly mock (`tests/testthat/helper-mock-ld.R`) and fake worker for parent-side IPC
- New suites for evaluation types, contexts, redaction, security, API contract, HTTP requests, worker resilience, event pipeline, worker lifecycle, fork safety, IPC and store round-trips
- Tautological assertions in the coverage tests replaced with exact expectations
- CI: R 4.1 (minimum supported) added to the check matrix

### DoD hardening (follow-up)
- FDv2 stream via curl multi SSE (httr2 blocking reads hang on open eventsource); tags charset + 64-char limit
- Polling initializers (ordered), stream-only init, 401/403 → secondary stream (`<<-` config fix)
- Streaming: discard in-flight payload on `error` event; trust change-set over model versions; IPC pump during long-lived stream
- Evaluator: legacy attribute refs, MALFORMED_FLAG for bad refs/cycles, RFC3339 dates, semver prerelease, bucketing scale
- Offline unit coverage ≥90% on evaluator / context / data_system / events; hard covr gate (no soft pass)
- Documented exclusion: `process-worker.R` / `inst/worker` (callr IPC invisible to covr; NOTES D24)
- GitHub Actions R CMD check matrix (OS × release/devel/oldrel) + hard covr gate
- CI soak (`scripts/soak-ci.R`) + documented 24h soak (`scripts/soak-24h.R`)
- R CMD check `--as-cran`: 0 errors / 0 warnings (2 environmental notes)
