#' Flag change listeners
#'
#' @export
FlagTracker <- R6::R6Class(
  "FlagTracker",
  public = list(
    initialize = function() {
      private$flag_listeners_ <- list()
      private$value_listeners_ <- list()
      invisible(self)
    },

    add_flag_change_listener = function(callback) {
      id <- private$next_id()
      private$flag_listeners_[[id]] <- callback
      id
    },

    add_flag_value_change_listener = function(flag_key, context, callback) {
      id <- private$next_id()
      private$value_listeners_[[id]] <- list(
        flag_key = flag_key, context = context, callback = callback, last = NULL
      )
      id
    },

    remove_listener = function(id) {
      private$flag_listeners_[[id]] <- NULL
      private$value_listeners_[[id]] <- NULL
      invisible(TRUE)
    },

    #' Notify after store update
    #' @param eval_fn function(flag_key, context) -> value
    notify_store_update = function(old_flags, new_flags, eval_fn = NULL) {
      changed <- character()
      keys <- union(names(old_flags), names(new_flags))
      for (k in keys) {
        if (!identical(old_flags[[k]], new_flags[[k]])) changed <- c(changed, k)
      }
      for (cb in private$flag_listeners_) {
        for (k in changed) try(cb(k), silent = TRUE)
      }
      if (is.function(eval_fn)) {
        for (id in names(private$value_listeners_)) {
          vl <- private$value_listeners_[[id]]
          if (!vl$flag_key %in% changed) next
          val <- eval_fn(vl$flag_key, vl$context)
          if (!identical(val, vl$last)) {
            try(vl$callback(list(key = vl$flag_key, oldValue = vl$last, newValue = val)), silent = TRUE)
            private$value_listeners_[[id]]$last <- val
          }
        }
      }
      invisible(changed)
    }
  ),
  private = list(
    flag_listeners_ = NULL,
    value_listeners_ = NULL,
    seq_ = 0L,
    next_id = function() {
      private$seq_ <- private$seq_ + 1L
      as.character(private$seq_)
    }
  )
)
