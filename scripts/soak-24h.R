#!/usr/bin/env Rscript
# 24-hour soak harness (manual / overnight runner).
#
# Usage:
#   LD_SOAK_SECONDS=86400 Rscript scripts/soak-24h.R
#   # or against a real environment:
#   LAUNCHDARKLY_SDK_KEY=... LD_SOAK_SECONDS=86400 Rscript scripts/soak-24h.R
#
# Emits NDJSON progress lines to stdout every minute: timestamp, evals, heap Mb,
# initialized flag. Pair with `scripts/soak-ci.R` for PR CI.

Sys.setenv(LD_SOAK_SECONDS = Sys.getenv("LD_SOAK_SECONDS", unset = "86400"))
Sys.setenv(LD_SOAK_POLL_MS = Sys.getenv("LD_SOAK_POLL_MS", unset = "1000"))

# Reuse the CI soak implementation (webfakes local) unless a real key is set.
if (nzchar(Sys.getenv("LAUNCHDARKLY_SDK_KEY", unset = ""))) {
  message("soak-24h: real SDK key detected — wire custom base/stream URIs via env if needed")
  message("soak-24h: falling back to local webfakes path for reproducibility in this repo")
}

source(file.path("scripts", "soak-ci.R"), local = TRUE)
