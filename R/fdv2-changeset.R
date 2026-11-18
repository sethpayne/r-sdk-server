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
  if (!is.list(data) || is.null(names(data))) {
    return(list(error = "Invalid payload: expected a JSON object"))
  }
  if (is.null(data$events) || !is.list(data$events)) {
    return(list(error = "Invalid payload: 'events' key is missing or not a list"))
  }

  intent <- NULL
  puts <- list()
  deletes <- list()

  for (event in data$events) {
    if (!is.list(event) || is.null(event$event)) next
    ev <- event$event

    if (!is.character(ev) || length(ev) != 1L) next
    if (identical(ev, LD_EVENT_SERVER_INTENT)) {
      # Wire format: { payloads: [ { intentCode, id, target, reason } ] }
      if (!is.null(event$data) && (!is.list(event$data) || is.null(names(event$data)))) {
        return(list(error = "Invalid payload: 'server-intent' data must be an object"))
      }
      payloads <- event$data$payloads
      code <- NULL
      if (is.list(payloads) && length(payloads) >= 1L && is.list(payloads[[1]])) {
        code <- payloads[[1]]$intentCode %||% payloads[[1]]$code
      }
      if (is.null(code) && is.list(event$data$payload)) code <- event$data$payload$code
      code <- code %||% event$data$code
      if (!is.null(code) && !(is.character(code) && length(code) == 1L)) {
        return(list(error = "Invalid payload: 'server-intent' intent code must be a string"))
      }
      if (identical(code, LD_INTENT_TRANSFER_NONE)) {
        return(list(no_changes = TRUE))
      }
      intent <- code %||% LD_INTENT_TRANSFER_FULL
    } else if (identical(ev, LD_EVENT_PUT_OBJECT)) {
      ed <- event$data
      if (!is.list(ed) || is.null(names(ed)) || !is.character(ed$kind) || length(ed$kind) != 1L) {
        return(list(error = "Invalid payload: 'put-object' data must be an object"))
      }
      if (!ed$kind %in% .ld_supported_kinds) next
      if (!.ld_valid_key(ed$key)) {
        return(list(error = "Invalid payload: 'put-object' requires a string key"))
      }
      obj <- ed$object
      # json.RawMessage may arrive as a JSON string — parse before upsert.
      if (is.character(obj) && length(obj) == 1L && nzchar(obj)) {
        obj <- tryCatch(jsonlite::fromJSON(obj, simplifyVector = FALSE), error = function(e) obj)
      }
      puts[[length(puts) + 1L]] <- list(
        kind = ed$kind,
        key = ed$key,
        version = ed$version,
        object = obj
      )
    } else if (identical(ev, LD_EVENT_DELETE_OBJECT)) {
      ed <- event$data
      if (!is.list(ed) || is.null(names(ed)) || !is.character(ed$kind) || length(ed$kind) != 1L) {
        return(list(error = "Invalid payload: 'delete-object' data must be an object"))
      }
      if (!ed$kind %in% .ld_supported_kinds) next
      if (!.ld_valid_key(ed$key)) {
        return(list(error = "Invalid payload: 'delete-object' requires a string key"))
      }
      deletes[[length(deletes) + 1L]] <- list(
        kind = ed$kind,
        key = ed$key,
        version = ed$version
      )
    } else if (identical(ev, LD_EVENT_PAYLOAD_TRANSFERRED)) {
      sel <- event$data
      if (!is.list(sel) || is.null(names(sel)) || is.null(sel$state) || is.null(sel$version)) {
        return(list(error = "Invalid JSON in payload transferred object"))
      }
      # Streaming deltas without a fresh server-intent are changes, not full replaces.
      return(list(
        intent = intent %||% LD_INTENT_TRANSFER_CHANGES,
        puts = puts,
        deletes = deletes,
        selector = list(state = as.character(sel$state), version = as.integer(sel$version))
      ))
    }
  }

  list(error = "didn't receive any known protocol events in polling payload")
}
