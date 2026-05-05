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

testthat::test_that("model help observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  modelHelpText = readPackageText("R", "app-server-model-help.R")

  testthat::expect_true(grepl("registerModelHelpObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelHelpObservers = function", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("rv = rv", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("output$modelHelpBtnUi", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("output$userDatasetContextUi", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("output$modelHelpModalBody", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$modelHelpBtn", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$saveUserDatasetContextBtn", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$researchQuestion", modelHelpText, fixed = TRUE))
  testthat::expect_false(grepl("output$modelHelpBtnUi = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("output$modelHelpModalBody = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("observeEvent(input$modelHelpBtn", appServerText, fixed = TRUE))
})
