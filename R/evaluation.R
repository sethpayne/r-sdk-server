#' Evaluation result types
#'
#' @name evaluation
NULL

#' Create an evaluation detail
#' @param value Evaluated value.
#' @param variation_index 0-based variation index or `NULL`.
#' @param reason Named list reason.
#' @export
evaluation_detail <- function(value, variation_index = NULL, reason = list(kind = "OFF")) {
  structure(
    list(value = value, variation_index = variation_index, reason = reason),
    class = "ld_evaluation_detail"
  )
}

#' @export
print.ld_evaluation_detail <- function(x, ...) {
  cat(sprintf(
    "EvaluationDetail(value=%s, variation_index=%s, reason=%s)\n",
    paste(utils::capture.output(str(x$value, give.head = FALSE, no.list = TRUE)), collapse = " "),
    if (is.null(x$variation_index)) "NULL" else as.character(x$variation_index),
    jsonlite::toJSON(x$reason, auto_unbox = TRUE)
  ))
  invisible(x)
}

#' Feature flags state (bootstrap)
#' @export
FeatureFlagsState <- R6::R6Class(
  "FeatureFlagsState",
  public = list(
    initialize = function(valid = FALSE) {
      private$values_ <- list()
      private$meta_ <- list()
      private$valid_ <- isTRUE(valid)
      invisible(self)
    },
    valid = function() private$valid_,
    get_flag_value = function(key) private$values_[[key]],
    get_flag_reason = function(key) {
      m <- private$meta_[[key]]
      if (is.null(m)) NULL else m$reason
    },
    add_flag = function(key, value, variation = NULL, version = NULL, reason = NULL,
                        track_events = FALSE, track_reason = FALSE,
                        debug_events_until_date = NULL) {
      # R list[[key]] <- NULL removes the key; preserve JSON null via list(NULL)
      if (is.null(value)) {
        private$values_[key] <- list(NULL)
      } else {
        private$values_[[key]] <- value
      }
      meta <- list()
      if (!is.null(variation)) meta$variation <- as.integer(variation)
      if (!is.null(version)) meta$version <- as.integer(version)
      if (!is.null(reason)) meta$reason <- reason
      if (isTRUE(track_events)) meta$trackEvents <- TRUE
      if (isTRUE(track_reason)) meta$trackReason <- TRUE
      if (!is.null(debug_events_until_date) && !identical(debug_events_until_date, NULL)) {
        meta$debugEventsUntilDate <- debug_events_until_date
      }
      # Drop NULL-ish empties for compact representation
      private$meta_[[key]] <- meta
      invisible(self)
    },
    to_json_list = function() {
      ret <- private$values_
      # Omit empty meta objects' null fields already; ensure list not empty array
      meta <- private$meta_
      if (length(meta) == 0L) meta <- setNames(list(), character())
      ret[["$flagsState"]] <- meta
      ret[["$valid"]] <- private$valid_
      ret
    }
  ),
  private = list(
    values_ = NULL,
    meta_ = NULL,
    valid_ = FALSE
  )
)
