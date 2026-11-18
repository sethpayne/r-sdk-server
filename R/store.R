#' In-memory flag/segment store (parent snapshot + worker authority)
#'
#' @keywords internal
NULL

#' @export
FeatureStore <- R6::R6Class(
  "FeatureStore",
  public = list(
    initialize = function() {
      private$flags_ <- new.env(parent = emptyenv(), hash = TRUE)
      private$segments_ <- new.env(parent = emptyenv(), hash = TRUE)
      private$selector_ <- list(state = "", version = 0L)
      private$initialized_ <- FALSE
      invisible(self)
    },

    is_initialized = function() isTRUE(private$initialized_),

    mark_initialized = function(value = TRUE) {
      private$initialized_ <- isTRUE(value)
      invisible(self)
    },

    get_flag = function(key) {
      if (exists(key, envir = private$flags_, inherits = FALSE)) {
        get(key, envir = private$flags_, inherits = FALSE)
      } else {
        NULL
      }
    },

    get_segment = function(key) {
      if (exists(key, envir = private$segments_, inherits = FALSE)) {
        get(key, envir = private$segments_, inherits = FALSE)
      } else {
        NULL
      }
    },

    all_flags = function() {
      keys <- ls(private$flags_, all.names = TRUE)
      setNames(lapply(keys, function(k) get(k, envir = private$flags_)), keys)
    },

    selector = function() private$selector_,

    set_selector = function(state, version) {
      private$selector_ <- list(state = as.character(state), version = as.integer(version))
      invisible(self)
    },

    upsert = function(kind, key, version, object) {
      env <- private$env_for(kind)
      if (is.null(env)) return(invisible(FALSE))
      # Prefer version embedded in the object (flag/segment JSON); envelope
      # version is a change-set sequence that may differ.
      obj_ver <- object$version %||% version
      if (exists(key, envir = env, inherits = FALSE)) {
        cur <- get(key, envir = env, inherits = FALSE)
        cur_ver <- cur$version %||% -1L
        if (!is.null(cur_ver) && as.integer(obj_ver) < as.integer(cur_ver) &&
            as.integer(version) < as.integer(cur_ver)) {
          return(invisible(FALSE))
        }
      }
      obj <- object
      if (is.null(obj$key)) obj$key <- key
      if (is.null(obj$version)) obj$version <- as.integer(version)
      assign(key, obj, envir = env)
      invisible(TRUE)
    },

    delete = function(kind, key, version) {
      env <- private$env_for(kind)
      if (is.null(env)) return(invisible(FALSE))
      if (exists(key, envir = env, inherits = FALSE)) {
        cur <- get(key, envir = env, inherits = FALSE)
        cur_ver <- cur$version %||% -1L
        if (!is.null(cur_ver) && as.integer(version) < as.integer(cur_ver)) {
          return(invisible(FALSE))
        }
      }
      # Tombstone
      assign(key, list(key = key, version = as.integer(version), deleted = TRUE), envir = env)
      invisible(TRUE)
    },

    #' Replace store from a full snapshot payload (list with flags/segments).
    replace_all = function(flags = list(), segments = list(), selector = NULL) {
      private$flags_ <- new.env(parent = emptyenv(), hash = TRUE)
      private$segments_ <- new.env(parent = emptyenv(), hash = TRUE)
      for (nm in names(flags)) {
        assign(nm, flags[[nm]], envir = private$flags_)
      }
      for (nm in names(segments)) {
        assign(nm, segments[[nm]], envir = private$segments_)
      }
      if (!is.null(selector)) {
        private$selector_ <- list(
          state = as.character(selector$state %||% ""),
          version = as.integer(selector$version %||% 0L)
        )
      }
      private$initialized_ <- TRUE
      invisible(self)
    },

    #' Apply a parsed change set (list with intent, puts, deletes, selector).
    apply_changeset = function(cs) {
      if (is.null(cs) || isTRUE(cs$no_changes)) {
        return(invisible(self))
      }
      intent <- cs$intent %||% "xfer-full"
      if (identical(intent, "xfer-full")) {
        private$flags_ <- new.env(parent = emptyenv(), hash = TRUE)
        private$segments_ <- new.env(parent = emptyenv(), hash = TRUE)
      }
      for (p in cs$puts %||% list()) {
        self$upsert(p$kind, p$key, p$version, p$object)
      }
      for (d in cs$deletes %||% list()) {
        self$delete(d$kind, d$key, d$version)
      }
      if (!is.null(cs$selector)) {
        self$set_selector(cs$selector$state, cs$selector$version)
      }
      private$initialized_ <- TRUE
      invisible(self)
    },

    to_snapshot = function() {
      list(
        flags = self$all_flags(),
        segments = {
          keys <- ls(private$segments_, all.names = TRUE)
          setNames(lapply(keys, function(k) get(k, envir = private$segments_)), keys)
        },
        selector = private$selector_
      )
    }
  ),
  private = list(
    flags_ = NULL,
    segments_ = NULL,
    selector_ = NULL,
    initialized_ = FALSE,
    env_for = function(kind) {
      if (identical(kind, LD_KIND_FLAG) || identical(kind, "features")) return(private$flags_)
      if (identical(kind, LD_KIND_SEGMENT) || identical(kind, "segments")) return(private$segments_)
      NULL
    }
  )
)
