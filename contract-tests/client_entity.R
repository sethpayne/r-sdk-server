# Client entity for sdk-test-harness v3.

`%||%` <- function(a, b) if (is.null(a)) b else a

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

      opts <- list(
        sdk_key = configuration$credential %||% "",
        send_events = FALSE,
        start_wait = 5
      )
      if (isTRUE(configuration$offline)) opts$offline <- TRUE

      endpoints <- configuration$serviceEndpoints %||% list()
      if (!is.null(endpoints$polling)) opts$base_uri <- endpoints$polling
      if (!is.null(endpoints$streaming)) opts$stream_uri <- endpoints$streaming
      if (!is.null(endpoints$events)) opts$events_uri <- endpoints$events

      # Legacy streaming/polling config blocks
      if (!is.null(configuration$polling)) {
        opts$stream <- FALSE
        pol <- configuration$polling
        if (!is.null(pol$baseUri)) opts$base_uri <- pol$baseUri
        if (!is.null(pol$pollIntervalMs)) opts$poll_interval <- pol$pollIntervalMs / 1000
      }
      if (!is.null(configuration$streaming)) {
        opts$stream <- TRUE
        st <- configuration$streaming
        if (!is.null(st$baseUri)) opts$stream_uri <- st$baseUri
        if (!is.null(st$initialRetryDelayMs)) {
          opts$initial_reconnect_delay <- st$initialRetryDelayMs / 1000
        }
      }

      # FDv2 dataSystem (harness v3)
      ds <- configuration$dataSystem
      if (!is.null(ds)) {
        inits <- ds$initializers %||% list()
        for (init in inits) {
          if (!is.null(init$polling)) {
            if (!is.null(init$polling$baseUri)) opts$base_uri <- init$polling$baseUri
            if (!is.null(init$polling$pollIntervalMs)) {
              opts$poll_interval <- init$polling$pollIntervalMs / 1000
            }
          }
        }
        syncs <- ds$synchronizers %||% list()
        if (length(syncs) >= 1L) {
          s0 <- syncs[[1]]
          if (!is.null(s0$streaming)) {
            opts$stream <- TRUE
            if (!is.null(s0$streaming$baseUri)) opts$stream_uri <- s0$streaming$baseUri
            if (!is.null(s0$streaming$initialRetryDelayMs)) {
              opts$initial_reconnect_delay <- s0$streaming$initialRetryDelayMs / 1000
            }
            # Stream-only when no polling initializer was provided
            if (length(inits) == 0L) {
              opts$stream_only <- TRUE
              opts$skip_poll_init <- TRUE
            }
          } else if (!is.null(s0$polling)) {
            opts$stream <- FALSE
            # Prefer synchronizer poll URI for ongoing sync; keep initializer URI
            # as base if set, else use sync poll URI for both.
            if (!is.null(s0$polling$baseUri)) {
              if (is.null(opts$base_uri)) {
                opts$base_uri <- s0$polling$baseUri
              }
              opts$sync_poll_uri <- s0$polling$baseUri
              # When there is no separate initializer, sync URI is also the init URI.
              if (length(inits) == 0L) opts$base_uri <- s0$polling$baseUri
            }
            if (!is.null(s0$polling$pollIntervalMs)) {
              opts$poll_interval <- s0$polling$pollIntervalMs / 1000
            }
          }
        }
        # Optional second synchronizer (poll or stream fallback)
        if (length(syncs) >= 2L) {
          if (!is.null(syncs[[2]]$polling$baseUri)) {
            opts$fallback_poll_uri <- syncs[[2]]$polling$baseUri
          }
          if (!is.null(syncs[[2]]$streaming$baseUri)) {
            opts$fallback_stream_uri <- syncs[[2]]$streaming$baseUri
          }
        }
      }

      if (!is.null(configuration$events)) {
        opts$send_events <- TRUE
        ev <- configuration$events
        if (!is.null(ev$baseUri)) opts$events_uri <- ev$baseUri
        if (!is.null(ev$capacity)) opts$events_capacity <- ev$capacity
        if (!is.null(ev$flushIntervalMs)) opts$flush_interval <- ev$flushIntervalMs / 1000
        opts$all_attributes_private <- isTRUE(ev$allAttributesPrivate)
        if (!is.null(ev$globalPrivateAttributes)) {
          opts$private_attributes <- unlist(ev$globalPrivateAttributes, use.names = FALSE)
        }
        opts$omit_anonymous_contexts <- isTRUE(ev$omitAnonymousContexts)
      }

      tags <- configuration$tags %||% list()
      if (!is.null(tags$applicationId)) opts$application_id <- tags$applicationId
      if (!is.null(tags$applicationVersion)) opts$application_version <- tags$applicationVersion

      wrapper <- configuration$wrapper %||% list()
      if (!is.null(wrapper$name)) opts$wrapper_name <- wrapper$name
      if (!is.null(wrapper$version)) opts$wrapper_version <- wrapper$version

      start_wait_ms <- configuration$startWaitTimeMs
      if (is.null(start_wait_ms)) start_wait_ms <- 5000

      # Short poll interval for tests when unset
      if (is.null(opts$poll_interval)) opts$poll_interval <- 0.5

      sdk_key <- opts$sdk_key
      opts$sdk_key <- NULL
      cfg <- do.call(launchdarklyServer::ld_config, c(list(sdk_key = sdk_key), opts))
      # Stash extra URIs on config for worker (optional fields)
      if (!is.null(opts$sync_poll_uri)) cfg$sync_poll_uri <- opts$sync_poll_uri
      if (!is.null(opts$fallback_poll_uri)) cfg$fallback_poll_uri <- opts$fallback_poll_uri
      if (!is.null(opts$fallback_stream_uri)) cfg$fallback_stream_uri <- opts$fallback_stream_uri
      if (isTRUE(opts$stream_only)) cfg$stream_only <- TRUE
      if (isTRUE(opts$skip_poll_init)) cfg$skip_poll_init <- TRUE

      self$client <- launchdarklyServer::ld_client(cfg, start_wait = start_wait_ms / 1000)
      self$initializing <- !isTRUE(self$client$is_initialized())
      invisible(self)
    },

    is_initializing = function() {
      isTRUE(self$initializing)
    },

    evaluate = function(params) {
      ctx <- launchdarklyServer::context_from_list(params$context)
      if (isTRUE(params$detail)) {
        d <- self$client$variation_detail(params$flagKey, ctx, params$defaultValue)
        list(
          value = d$value,
          variationIndex = d$variation_index,
          reason = d$reason
        )
      } else {
        list(value = self$client$variation(params$flagKey, ctx, params$defaultValue))
      }
    },

    evaluate_all = function(params) {
      ctx <- launchdarklyServer::context_from_list(params$context)
      state <- self$client$all_flags_state(
        ctx,
        client_side_only = isTRUE(params$clientSideOnly),
        with_reasons = isTRUE(params$withReasons),
        details_only_for_tracked_flags = isTRUE(params$detailsOnlyForTrackedFlags)
      )
      list(state = state$to_json_list())
    },

    track = function(params) {
      ctx <- launchdarklyServer::context_from_list(params$context)
      self$client$track(params$eventKey, ctx, params$data, params$metricValue)
      invisible(NULL)
    },

    identify = function(params) {
      ctx <- launchdarklyServer::context_from_list(params$context)
      self$client$identify(ctx)
      invisible(NULL)
    },

    flush = function() {
      self$client$flush()
      invisible(NULL)
    },

    secure_mode_hash = function(params) {
      ctx <- launchdarklyServer::context_from_list(params$context)
      list(result = self$client$secure_mode_hash(ctx))
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
      if (!is.null(self$client)) self$client$close()
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
