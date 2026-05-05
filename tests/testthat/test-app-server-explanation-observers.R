findPackageRootForExplanationObserverTests = function(startDir = getwd()) {
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

readPackageTextForExplanationObserverTests = function(...) {
  packageRoot = findPackageRootForExplanationObserverTests()

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

testthat::test_that("model explanation observers are registered from app server", {
  appServerText = readPackageTextForExplanationObserverTests("R", "app-server.R")
  explanationText = readPackageTextForExplanationObserverTests("R", "app-server-explanation.R")

  testthat::expect_true(grepl("registerModelExplanationObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelExplanationObservers = function", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("output$model_explanation = renderUI", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$modelExplanationTutorBtn", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("output$developerFeedbackReportDownload", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("modelExplanationTeachingSummary", explanationText, fixed = TRUE))
  testthat::expect_false(grepl("output$model_explanation = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("observeEvent(input$modelExplanationTutorBtn", appServerText, fixed = TRUE))
})
