#!/usr/bin/env Rscript
# Coverage gate for v1 DoD: ≥90% on evaluator, context, data system (store/FDv2), events.
# Unit tests use offline mode / webfakes only (no live network).

suppressPackageStartupMessages({
  library(covr)
})

if (!file.exists("DESCRIPTION")) {
  stop("Run from package root (DESCRIPTION not found)", call. = FALSE)
}

cat("Computing package coverage (testthat)...\n")
cov <- package_coverage(type = "tests", quiet = TRUE)
cat(sprintf("Overall package coverage: %.1f%%\n", percent_coverage(cov)))

tally <- tally_coverage(cov)
by_file <- tapply(
  seq_len(nrow(tally)),
  tally$filename,
  function(idx) {
    v <- tally$value[idx]
    100 * sum(v > 0, na.rm = TRUE) / length(v)
  }
)

match_files <- function(basenames) {
  unique(unlist(lapply(basenames, function(b) {
    names(by_file)[grepl(paste0(b, "$"), names(by_file), fixed = FALSE)]
  })))
}

group_pct <- function(basenames) {
  matched <- match_files(basenames)
  if (length(matched) == 0L) return(NA_real_)
  vals <- tally$value[tally$filename %in% matched]
  100 * sum(vals > 0, na.rm = TRUE) / length(vals)
}

groups <- list(
  evaluator = c("evaluator.R", "operators.R", "evaluation.R"),
  context = c("context.R"),
  data_system = c("store.R", "fdv2-changeset.R", "fdv2-polling.R", "fdv2-streaming.R"),
  event_processor = c("events.R")
)

cat("\nDoD group coverage:\n")
failures <- character()
for (nm in names(groups)) {
  pct <- group_pct(groups[[nm]])
  cat(sprintf("  %-18s %s\n", nm, if (is.na(pct)) "n/a" else sprintf("%.1f%%", pct)))
  if (is.na(pct) || pct < 90) {
    failures <- c(failures, sprintf("%s (%.1f%%)", nm, pct))
  }
}

if (length(failures)) {
  cat("\nPer-file coverage (debug):\n")
  print(sort(by_file))
  cat("\nCoverage gate FAILED:\n")
  cat(paste(" -", failures), sep = "\n")
  if (identical(Sys.getenv("LD_COVERAGE_SOFT", unset = ""), "1")) {
    cat("\nLD_COVERAGE_SOFT=1 — reporting failure but exiting 0 (worker IPC coverage invisible to covr).\n")
    quit(status = 0)
  }
  quit(status = 1)
}

cat("\nCoverage gate PASSED (≥90% on evaluator/context/data_system/event_processor).\n")
