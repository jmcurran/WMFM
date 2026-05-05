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

test_that("model plot observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  plotObserverText = readPackageText("R", "app-server-plot.R")

  expect_true(grepl("registerModelPlotObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerModelPlotObservers = function", plotObserverText, fixed = TRUE))
  expect_true(grepl("modelFit = modelFit", appServerText, fixed = TRUE))
  expect_true(grepl("output$plot_ci_controls_ui", plotObserverText, fixed = TRUE))
  expect_true(grepl("output$model_plot", plotObserverText, fixed = TRUE))
  expect_true(grepl("drawModelPlot", plotObserverText, fixed = TRUE))
  expect_false(grepl("output$plot_ci_controls_ui = renderUI", appServerText, fixed = TRUE))
  expect_false(grepl("output$model_plot = renderPlot", appServerText, fixed = TRUE))
})
