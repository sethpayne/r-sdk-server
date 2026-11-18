test_that("contract service contextBuild/contextConvert smoke", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("httr2")

  port <- httpuv::randomPort()
  repo <- normalizePath(testthat::test_path("../.."))
  service_script <- file.path(repo, "contract-tests", "service.R")

  # Start service in background process
  proc <- callr::r_bg(
    function(script, port) {
      .libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
      commandArgs <- function(trailingOnly = FALSE) {
        if (trailingOnly) as.character(port) else c(paste0("--file=", script))
      }
      # callr cannot override commandArgs easily; exec via system2 instead
    },
    args = list(service_script, port),
    supervise = TRUE
  )
  # Prefer system2/Rscript for a real script invocation
  if (proc$is_alive()) proc$kill()

  log_file <- tempfile("cts")
  px <- processx::process$new(
    "Rscript",
    c(service_script, as.character(port)),
    stdout = log_file,
    stderr = "2>&1",
    supervise = TRUE
  )
  on.exit({
    if (px$is_alive()) px$kill(close_connections = TRUE)
  }, add = TRUE)

  # Wait until listening
  deadline <- Sys.time() + 15
  ready <- FALSE
  while (Sys.time() < deadline) {
    ok <- tryCatch({
      resp <- httr2::req_perform(httr2::request(sprintf("http://127.0.0.1:%d/", port)))
      httr2::resp_status(resp) == 200
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      ready <- TRUE
      break
    }
    if (!px$is_alive()) break
    Sys.sleep(0.1)
  }
  expect_true(ready, info = paste(readLines(log_file, warn = FALSE), collapse = "\n"))

  status <- httr2::resp_body_json(httr2::req_perform(
    httr2::request(sprintf("http://127.0.0.1:%d/", port))
  ))
  expect_true("context-type" %in% unlist(status$capabilities))

  create <- httr2::req_perform(
    httr2::request(sprintf("http://127.0.0.1:%d/", port)) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(list(
        tag = "t",
        configuration = list(credential = "sdk-key", offline = TRUE, startWaitTimeMs = 0)
      ))
  )
  expect_equal(httr2::resp_status(create), 201)
  loc <- httr2::resp_header(create, "location")
  expect_true(grepl("^/clients/", loc))

  build <- httr2::req_perform(
    httr2::request(sprintf("http://127.0.0.1:%d%s", port, loc)) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(list(
        command = "contextBuild",
        contextBuild = list(single = list(key = "u1", kind = "user", name = "Ada"))
      ))
  )
  expect_equal(httr2::resp_status(build), 200)
  body <- httr2::resp_body_json(build)
  expect_true(!is.null(body$output))
  expect_true(grepl("Ada", body$output))

  convert <- httr2::req_perform(
    httr2::request(sprintf("http://127.0.0.1:%d%s", port, loc)) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(list(
        command = "contextConvert",
        contextConvert = list(input = '{"kind":"user","key":"u2"}')
      ))
  )
  expect_equal(httr2::resp_status(convert), 200)
  cbody <- httr2::resp_body_json(convert)
  expect_true(!is.null(cbody$output))

  del <- httr2::req_perform(
    httr2::request(sprintf("http://127.0.0.1:%d%s", port, loc)) |>
      httr2::req_method("DELETE")
  )
  expect_equal(httr2::resp_status(del), 202)
})
