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

test_that("fitted equation observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  fittedEquationObserverText = readPackageText("R", "app-server-fitted-equations.R")

  expect_true(grepl("registerFittedEquationObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerFittedEquationObservers = function", fittedEquationObserverText, fixed = TRUE))
  expect_true(grepl("output = output", appServerText, fixed = TRUE))
  expect_true(grepl("rv = rv", appServerText, fixed = TRUE))
  expect_true(grepl("modelFit = modelFit", appServerText, fixed = TRUE))
  expect_true(grepl("output$model_equations", fittedEquationObserverText, fixed = TRUE))
  expect_true(grepl("output$model_equations_header", fittedEquationObserverText, fixed = TRUE))
  expect_true(grepl("output$fitted_means", fittedEquationObserverText, fixed = TRUE))
  expect_false(grepl("output$model_equations = renderUI", appServerText, fixed = TRUE))
  expect_false(grepl("output$fitted_means = renderUI", appServerText, fixed = TRUE))
})
