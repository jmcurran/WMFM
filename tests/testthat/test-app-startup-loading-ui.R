findPackageRoot = function(startDir = getwd()) {
  startDir = normalizePath(startDir, winslash = "/", mustWork = TRUE)

  candidates = unique(c(
    startDir,
    dirname(startDir),
    dirname(dirname(startDir)),
    dirname(dirname(dirname(startDir))),
    Sys.getenv("TESTTHAT_PKG_PATH", unset = "")
  ))


  candidates = candidates[nzchar(candidates)]

  for (candidate in candidates) {
    descriptionPath = file.path(candidate, "DESCRIPTION")
    rPath = file.path(candidate, "R")

    if (file.exists(descriptionPath) && dir.exists(rPath)) {
      return(candidate)
    }
  }

  NA_character_
}

readPackageText = function(...) {
  packageRoot = findPackageRoot()

  if (is.na(packageRoot)) {
    testthat::skip("Package source tree is not available in this test environment")
  }

  filePath = file.path(packageRoot, ...)

  if (!file.exists(filePath)) {
    testthat::skip(paste0(
      "Package source file is not available in this test environment: ",
      paste(..., collapse = "/")
    ))
  }

  paste(readLines(filePath, warn = FALSE), collapse = "\n")
}

test_that("startup data choice loading gives immediate visible feedback", {
  serverText = readPackageText("R", "app-server.R")
  uiText = readPackageText("R", "app-ui.R")

  expect_match(serverText, "Loading examples\\.\\.\\.", fixed = FALSE)
  expect_match(uiText, "Loading examples...", fixed = TRUE)
  expect_match(uiText, "Loading packages...", fixed = TRUE)
  expect_match(serverText, "Checking installed packages for datasets", fixed = TRUE)
  expect_match(uiText, "Loading datasets...", fixed = TRUE)
  expect_match(serverText, "Dataset choices will appear once the package scan has finished", fixed = TRUE)
  expect_match(serverText, "wmfm-startup-data-choices", fixed = TRUE)
  expect_match(serverText, "removeNotification\\(startupNotificationId\\)", fixed = FALSE)
})

test_that("package dataset selector shows a loading placeholder before scanning", {
  serverText = readPackageText("R", "app-server.R")

  expect_match(serverText, "Loading datasets\\.\\.\\.", fixed = FALSE)
  expect_match(serverText, "wmfm-package-dataset-list", fixed = TRUE)
  expect_match(serverText, "session\\$onFlushed\\(function\\(\\) \\{", fixed = FALSE)
})

test_that("model outputs use direct tabs without a duplicate summary-table header", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "Model outputs", fixed = TRUE)
  expect_match(uiText, "id = \"model_output_tabs\"", fixed = TRUE)
  expect_false(grepl("Summary table", uiText, fixed = TRUE))
  expect_false(grepl("id = \"model_outputs\"", uiText, fixed = TRUE))
  expect_false(grepl("Regression output", uiText, fixed = TRUE))
})

test_that("model type uses a compact selector", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "selectInput\\(\\s*\\\"model_type\\\"", perl = TRUE)
  expect_false(grepl("radioButtons\\(\\s*\\\"model_type\\\"", uiText, perl = TRUE))
})

test_that("example loading updates the compact model type selector", {
  serverText = readPackageText("R", "app-server.R")

  expect_match(serverText, "updateSelectInput\\(\\s*session,\\s*\\\"model_type\\\"", perl = TRUE)
  expect_false(grepl("updateRadioButtons\\(\\s*session,\\s*\\\"model_type\\\"", serverText, perl = TRUE))
})
