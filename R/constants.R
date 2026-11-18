#' Package and SDK identity constants
#'
#' **Single source of truth** for repository name, R package name, SDK name,
#' and version. The SDK name / user-agent product token remain pending
#' LaunchDarkly assignment; do not treat this package as official.
#'
#' @keywords internal
NULL

# --- Identity (edit only here) ------------------------------------------------

#' @rdname constants
#' @export
LD_GITHUB_REPO <- "r-server-sdk"

#' @rdname constants
#' @export
LD_R_PACKAGE <- "launchdarklyServer"

#' SDK product name for docs and user-agent.
#' Pending LaunchDarkly assignment — placeholder only.
#' @rdname constants
#' @export
LD_SDK_NAME <- "PENDING_LAUNCHDARKLY_ASSIGNMENT"

#' Package / SDK version (keep in sync with DESCRIPTION Version).
#' @rdname constants
#' @export
LD_SDK_VERSION <- "0.0.0.9000"

#' User-Agent product token until an official name is assigned.
#' @rdname constants
#' @export
LD_USER_AGENT <- paste0("RServerClient-Unofficial/", LD_SDK_VERSION)

# Actual GitHub remote for this workspace may differ (see NOTES.md).
#' @rdname constants
#' @export
LD_GITHUB_REMOTE_HINT <- "sethpayne/r-sdk-server"
