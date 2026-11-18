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
- FDv2 stream path `/sdk/stream` with connection SSE (multi-payload), tags sanitization, sync poll URI
- Evaluator: legacy attribute refs, MALFORMED_FLAG for bad refs/cycles, RFC3339 dates, semver prerelease, bucketing scale
- `all_flags_state`: null values, experimentation `trackReason`, CLIENT_NOT_READY
- GitHub Actions R CMD check matrix (OS × release/devel/oldrel) + covr gate script
- CI soak (`scripts/soak-ci.R`) + documented 24h soak (`scripts/soak-24h.R`)
- R CMD check `--as-cran`: 0 errors / 0 warnings (2 environmental notes)
