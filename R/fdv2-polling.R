#' FDv2 polling HTTP client
#'
#' @keywords internal
NULL

LD_FDV2_POLL_PATH <- "/sdk/poll"
LD_FD_FALLBACK_HEADER <- "x-ld-fd-fallback"
LD_ENVID_HEADER <- "x-ld-envid"

# Allowed tag value charset (sdk-test-harness allAllowedTagChars)
.ld_tag_value_re <- "^[._a-zA-Z0-9-]+$"
# sdk-test-harness maxTagValueLength
.ld_tag_value_max_len <- 64L

#' Validate and build `X-LaunchDarkly-Tags` header value
#' @keywords internal
.ld_tags_header_value <- function(config) {
  parts <- character()
  app_id <- config$application_id
  app_ver <- config$application_version
  id_ok <- is.character(app_id) && length(app_id) == 1L && nzchar(app_id) &&
    nchar(app_id, type = "chars") <= .ld_tag_value_max_len &&
    grepl(.ld_tag_value_re, app_id)
  ver_ok <- is.character(app_ver) && length(app_ver) == 1L && nzchar(app_ver) &&
    nchar(app_ver, type = "chars") <= .ld_tag_value_max_len &&
    grepl(.ld_tag_value_re, app_ver)
  if (id_ok) parts <- c(parts, paste0("application-id/", app_id))
  if (ver_ok) parts <- c(parts, paste0("application-version/", app_ver))
  if (length(parts) == 0L) return(NULL)
  paste(parts, collapse = " ")
}

#' Perform one FDv2 poll request
#'
#' @param config Config list (from [LdConfig]$to_list()).
#' @param selector Optional list(`state`, `version`).
#' @param base_uri Optional override for polling base (initializer vs sync).
#' @return List with `ok`, `changeset` / `error`, `headers`, `fallback_to_fdv1`.
#' @keywords internal
fdv2_poll_once <- function(config, selector = NULL, base_uri = NULL) {
  base <- sub("/$", "", base_uri %||% config$base_uri %||% "https://app.launchdarkly.com")
  url <- paste0(base, LD_FDV2_POLL_PATH)
  req <- httr2::request(url) |>
    httr2::req_method("GET") |>
    httr2::req_headers(
      Authorization = config$sdk_key,
      `User-Agent` = LD_USER_AGENT,
      Accept = "application/json"
    ) |>
    httr2::req_timeout(config$read_timeout %||% 15)

  tags <- .ld_tags_header_value(config)
  if (!is.null(tags)) {
    req <- httr2::req_headers(req, `X-LaunchDarkly-Tags` = tags)
  }

  query <- list()
  if (!is.null(selector) && nzchar(selector$state %||% "")) {
    query$basis <- selector$state
  }
  if (!is.null(config$payload_filter_key) && nzchar(config$payload_filter_key)) {
    query$filter <- config$payload_filter_key
  }
  if (length(query)) {
    req <- httr2::req_url_query(req, !!!query)
  }

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, error = conditionMessage(resp), fallback_to_fdv1 = FALSE))
  }

  headers <- httr2::resp_headers(resp)
  # Normalize header names to lower-case keys for lookup
  hnames <- tolower(names(headers))
  names(headers) <- hnames
  fallback <- identical(tolower(as.character(headers[[LD_FD_FALLBACK_HEADER]] %||% "")), "true")
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    return(list(
      ok = FALSE,
      error = sprintf("polling HTTP %s", status),
      status = status,
      headers = headers,
      fallback_to_fdv1 = fallback
    ))
  }

  data <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  cs <- fdv2_polling_payload_to_changeset(data)
  if (!is.null(cs$error)) {
    return(list(ok = FALSE, error = cs$error, headers = headers, fallback_to_fdv1 = fallback))
  }
  list(
    ok = TRUE,
    changeset = cs,
    headers = headers,
    environment_id = headers[[LD_ENVID_HEADER]],
    fallback_to_fdv1 = fallback
  )
}
