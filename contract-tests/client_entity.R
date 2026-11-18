# Client entity for sdk-test-harness (M3: context commands; later milestones fill in).

ClientEntity <- R6::R6Class(
  "ClientEntity",
  public = list(
    tag = NULL,
    config = NULL,
    client = NULL,
    initializing = TRUE,

    initialize = function(tag, configuration) {
      self$tag <- tag
      self$config <- configuration
      # M3: no real LdClient yet. Offline/stub so context-only suites can run.
      # M4+ will construct launchdarklyServer::LdClient from configuration.
      self$client <- NULL
      offline <- isTRUE(configuration$offline)
      init_can_fail <- isTRUE(configuration$initCanFail)
      # Treat as initialized for context-only / offline stub clients.
      self$initializing <- FALSE
      if (!offline && is.null(configuration$streaming) && is.null(configuration$polling) &&
          is.null(configuration$dataSystem) && is.null(configuration$serviceEndpoints)) {
        # No data source configured — still OK for contextBuild/contextConvert.
        self$initializing <- FALSE
      }
      invisible(self)
    },

    is_initializing = function() {
      isTRUE(self$initializing)
    },

    context_build = function(params) {
      if (!is.null(params$multi)) {
        b <- launchdarklyServer::context_multi_builder()
        for (c in params$multi) {
          b$add(private$build_single(c))
        }
        return(private$context_response(b$build()))
      }
      private$context_response(private$build_single(params$single))
    },

    context_convert = function(params) {
      input <- params$input
      c <- tryCatch(
        launchdarklyServer::context_from_json(input),
        error = function(e) e
      )
      if (inherits(c, "error")) {
        return(list(error = conditionMessage(c)))
      }
      private$context_response(c)
    },

    close = function() {
      if (!is.null(self$client) && is.function(self$client$close)) {
        self$client$close()
      }
      invisible(NULL)
    }
  ),
  private = list(
    build_single = function(params) {
      b <- launchdarklyServer::context_builder(params$key)
      if (!is.null(params$kind)) b$kind(params$kind)
      if (!is.null(params$name)) b$name(params$name)
      if (!is.null(params$anonymous)) b$anonymous(isTRUE(params$anonymous))
      if (!is.null(params$custom)) {
        for (nm in names(params$custom)) {
          b$set(nm, params$custom[[nm]])
        }
      }
      if (!is.null(params$private)) {
        do.call(b$mark_private, as.list(params$private))
      }
      b$build()
    },
    context_response = function(c) {
      if (launchdarklyServer::context_valid(c)) {
        list(output = as.character(launchdarklyServer::context_to_json(c)))
      } else {
        list(error = launchdarklyServer::context_error(c))
      }
    }
  )
)
