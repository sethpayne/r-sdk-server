#!/usr/bin/env Rscript
# sdk-test-harness v3 contract test service (httpuv).
# M3: status + client lifecycle + contextBuild / contextConvert.

suppressPackageStartupMessages({
  library(httpuv)
  library(jsonlite)
  library(R6)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 0L)) b else a

# Resolve paths relative to this script
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}
repo_root <- normalizePath(file.path(script_dir, ".."))

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(repo_root, quiet = TRUE)
} else {
  library(launchdarklyServer)
}

source(file.path(script_dir, "client_entity.R"))

default_port <- 8000L
args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1L) as.integer(args[[1]]) else default_port

client_counter <- 0L
clients <- new.env(parent = emptyenv())

capabilities <- c(
  "server-side",
  "server-side-polling",
  "context-type",
  "all-flags-with-reasons",
  "all-flags-client-side-only",
  "all-flags-details-only-for-tracked-flags",
  "secure-mode-hash",
  "inline-context-all",
  "tags",
  "service-endpoints",
  "flag-change-listeners",
  "flag-value-change-listeners"
)

json_response <- function(status, body, headers = list()) {
  payload <- if (is.null(body)) {
    ""
  } else if (is.character(body) && length(body) == 1L && !is.list(body)) {
    body
  } else {
    as.character(toJSON(body, auto_unbox = TRUE, null = "null"))
  }
  h <- c(list("Content-Type" = "application/json"), headers)
  list(status = as.integer(status), headers = h, body = payload)
}

empty_response <- function(status, headers = list()) {
  list(status = as.integer(status), headers = headers, body = "")
}

read_json_body <- function(req) {
  raw <- req$rook.input$read()
  if (is.null(raw) || length(raw) == 0L) return(list())
  fromJSON(rawToChar(raw), simplifyVector = FALSE)
}

app <- function(req) {
  method <- toupper(req$REQUEST_METHOD)
  path <- req$PATH_INFO %||% "/"

  if (identical(method, "GET") && identical(path, "/")) {
    return(json_response(200L, list(
      name = "r-server-sdk",
      capabilities = as.list(capabilities)
    )))
  }

  if (identical(method, "DELETE") && identical(path, "/")) {
    message("Test service has told us to exit")
    later::later(function() quit(save = "no", status = 0), delay = 0.05)
    return(empty_response(200L))
  }

  if (identical(method, "POST") && identical(path, "/")) {
    options <- read_json_body(req)
    client_counter <<- client_counter + 1L
    client_id <- as.character(client_counter)
    resource_url <- paste0("/clients/", client_id)
    tag <- options$tag %||% client_id
    configuration <- options$configuration %||% list()
    entity <- ClientEntity$new(tag, configuration)
    if (isTRUE(entity$is_initializing()) && !isTRUE(configuration$initCanFail)) {
      entity$close()
      return(list(
        status = 500L,
        headers = list("Content-Type" = "text/plain"),
        body = "Failed to initialize"
      ))
    }
    assign(client_id, entity, envir = clients)
    return(empty_response(201L, list(Location = resource_url)))
  }

  m <- regexec("^/clients/([^/]+)$", path)
  parts <- regmatches(path, m)[[1]]
  if (length(parts) == 2L) {
    id <- parts[[2]]
    if (!exists(id, envir = clients, inherits = FALSE)) {
      return(empty_response(404L))
    }
    entity <- get(id, envir = clients, inherits = FALSE)

    if (identical(method, "DELETE")) {
      entity$close()
      rm(list = id, envir = clients)
      return(empty_response(202L))
    }

    if (identical(method, "POST")) {
      params <- read_json_body(req)
      command <- params$command
      sub <- params[[command]]

      response <- NULL
      if (identical(command, "evaluate")) {
        response <- entity$evaluate(sub)
      } else if (identical(command, "evaluateAll")) {
        response <- entity$evaluate_all(sub)
      } else if (identical(command, "customEvent")) {
        entity$track(sub)
      } else if (identical(command, "identifyEvent")) {
        entity$identify(sub)
      } else if (identical(command, "flushEvents")) {
        entity$flush()
      } else if (identical(command, "secureModeHash")) {
        response <- entity$secure_mode_hash(sub)
      } else if (identical(command, "contextBuild")) {
        response <- entity$context_build(sub)
      } else if (identical(command, "contextConvert")) {
        response <- entity$context_convert(sub)
      } else {
        return(empty_response(400L))
      }

      if (is.null(response)) {
        return(empty_response(201L))
      }
      return(json_response(200L, response))
    }
  }

  empty_response(404L)
}

message(sprintf("Listening on port %d", port))
s <- startServer("0.0.0.0", port, list(call = app))
on.exit(try(stopServer(s), silent = TRUE), add = TRUE)

while (TRUE) {
  service(timeoutMs = 50)
}
