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

testthat::test_that("model setup observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  modelSetupText = readPackageText("R", "app-server-model-setup.R")

  testthat::expect_true(grepl("registerModelSetupObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelSetupObservers = function", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("setBucketState = setBucketState", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("output$var_buckets = renderUI", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("output$interaction_ui = renderUI", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$factors", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$confirmAddDerivedVarBtn", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("output$response_picker = renderUI", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("updateTextInput(session, \"formula_text\"", modelSetupText, fixed = TRUE))
  testthat::expect_false(grepl("output$var_buckets = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("output$response_picker = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("observeEvent(input$confirmAddDerivedVarBtn", appServerText, fixed = TRUE))
})
