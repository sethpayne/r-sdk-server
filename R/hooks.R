#' Evaluation hooks (optional; open-sdk-specs HOOK-*)
#'
#' Minimal hook interface for M8. Not required for core harness suites.
#'
#' @export
LdHook <- R6::R6Class(
  "LdHook",
  public = list(
    name = NULL,
    initialize = function(name = "hook") {
      self$name <- name
      invisible(self)
    },
    before_evaluation = function(series_context) series_context,
    after_evaluation = function(series_context, detail) detail
  )
)

#' Run hooks around an evaluation
#' @keywords internal
.ld_with_hooks <- function(hooks, series_context, eval_fn) {
  ctx <- series_context
  for (h in hooks) {
    ctx <- tryCatch(h$before_evaluation(ctx), error = function(e) ctx)
  }
  detail <- eval_fn(ctx)
  for (h in hooks) {
    detail <- tryCatch(h$after_evaluation(ctx, detail), error = function(e) detail)
  }
  detail
}
