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
