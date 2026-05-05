findPackageRootForModelOutputObserverTests = function(startDir = getwd()) {
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

readPackageTextForModelOutputObserverTests = function(...) {
  packageRoot = findPackageRootForModelOutputObserverTests()

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

testthat::test_that("model summary output observers are registered from app server", {
  appServerText = readPackageTextForModelOutputObserverTests("R", "app-server.R")
  modelOutputText = readPackageTextForModelOutputObserverTests("R", "app-server-model-output.R")

  testthat::expect_true(grepl("registerModelSummaryObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelSummaryObservers = function", modelOutputText, fixed = TRUE))
  testthat::expect_true(grepl("output$model_output = renderPrint", modelOutputText, fixed = TRUE))
  testthat::expect_true(grepl("No model fitted yet.", modelOutputText, fixed = TRUE))
  testthat::expect_false(grepl("output$model_output = renderPrint", appServerText, fixed = TRUE))
})
