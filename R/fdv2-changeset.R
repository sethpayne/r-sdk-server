#' FDv2 change-set parsing (polling / streaming payloads)
#'
#' @keywords internal
NULL

LD_EVENT_SERVER_INTENT <- "server-intent"
LD_EVENT_PUT_OBJECT <- "put-object"
LD_EVENT_DELETE_OBJECT <- "delete-object"
LD_EVENT_PAYLOAD_TRANSFERRED <- "payload-transferred"
LD_INTENT_TRANSFER_FULL <- "xfer-full"
LD_INTENT_TRANSFER_CHANGES <- "xfer-changes"
LD_INTENT_TRANSFER_NONE <- "xfer-none"

.ld_supported_kinds <- c(LD_KIND_FLAG, LD_KIND_SEGMENT)

#' Parse an FDv2 polling JSON payload into a change set list
#'
#' @param data Parsed list with `events` array.
#' @return List with `intent`, `puts`, `deletes`, `selector`, or `no_changes`,
#'   or a list with `$error`.
#' @keywords internal
fdv2_polling_payload_to_changeset <- function(data) {
  if (is.null(data$events) || !is.list(data$events)) {
    return(list(error = "Invalid payload: 'events' key is missing or not a list"))
  }

  intent <- NULL
  puts <- list()
  deletes <- list()

  for (event in data$events) {
    if (!is.list(event) || is.null(event$event)) next
    ev <- event$event

    if (identical(ev, LD_EVENT_SERVER_INTENT)) {
      code <- event$data$payload$code %||% event$data$code
      if (identical(code, LD_INTENT_TRANSFER_NONE)) {
        return(list(no_changes = TRUE))
      }
      intent <- code %||% LD_INTENT_TRANSFER_FULL
    } else if (identical(ev, LD_EVENT_PUT_OBJECT)) {
      ed <- event$data
      if (!is.list(ed) || !is.character(ed$kind)) {
        return(list(error = "Invalid payload: 'put-object' data must be an object"))
      }
      if (!ed$kind %in% .ld_supported_kinds) next
      puts[[length(puts) + 1L]] <- list(
        kind = ed$kind,
        key = ed$key,
        version = ed$version,
        object = ed$object
      )
    } else if (identical(ev, LD_EVENT_DELETE_OBJECT)) {
      ed <- event$data
      if (!is.list(ed) || !is.character(ed$kind)) {
        return(list(error = "Invalid payload: 'delete-object' data must be an object"))
      }
      if (!ed$kind %in% .ld_supported_kinds) next
      deletes[[length(deletes) + 1L]] <- list(
        kind = ed$kind,
        key = ed$key,
        version = ed$version
      )
    } else if (identical(ev, LD_EVENT_PAYLOAD_TRANSFERRED)) {
      sel <- event$data
      if (is.null(sel$state) || is.null(sel$version)) {
        return(list(error = "Invalid JSON in payload transferred object"))
      }
      return(list(
        intent = intent %||% LD_INTENT_TRANSFER_FULL,
        puts = puts,
        deletes = deletes,
        selector = list(state = as.character(sel$state), version = as.integer(sel$version))
      ))
    }
  }

  list(error = "didn't receive any known protocol events in polling payload")
}
