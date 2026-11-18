#' In-memory test data source for unit tests and harness overrides (lightweight)
#'
#' @export
TestData <- R6::R6Class(
  "TestData",
  public = list(
    initialize = function() {
      private$flags_ <- list()
      private$listeners_ <- list()
      invisible(self)
    },

    #' @param key Flag key.
    #' @param ... Flag fields (on, variations, fallthrough, etc.).
    flag = function(key, ...) {
      extras <- list(...)
      flag <- c(list(key = key, version = (private$flags_[[key]]$version %||% 0L) + 1L), extras)
      if (is.null(flag$variations)) flag$variations <- list(FALSE, TRUE)
      if (is.null(flag$on)) flag$on <- TRUE
      if (is.null(flag$fallthrough)) flag$fallthrough <- list(variation = 0L)
      private$flags_[[key]] <- flag
      private$notify(key, flag)
      invisible(self)
    },

    update = function(key, ...) {
      self$flag(key, ...)
    },

    delete = function(key) {
      ver <- (private$flags_[[key]]$version %||% 0L) + 1L
      private$flags_[[key]] <- list(key = key, version = ver, deleted = TRUE)
      private$notify(key, private$flags_[[key]])
      invisible(self)
    },

    all_flags = function() private$flags_,

    #' Apply into an LdClient store (parent process).
    apply_to_client = function(client) {
      client$.update_store_for_tests(flags = private$flags_)
      invisible(self)
    },

    on_change = function(callback) {
      private$listeners_[[length(private$listeners_) + 1L]] <- callback
      invisible(self)
    }
  ),
  private = list(
    flags_ = NULL,
    listeners_ = NULL,
    notify = function(key, flag) {
      for (cb in private$listeners_) {
        try(cb(key, flag), silent = TRUE)
      }
    }
  )
)

#' @export
ld_test_data <- function() TestData$new()
