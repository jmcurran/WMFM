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

test_that("model formula observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  formulaObserverText = readPackageText("R", "app-server-formula.R")

  expect_true(grepl("registerModelFormulaObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerModelFormulaObservers = function", formulaObserverText, fixed = TRUE))
  expect_true(grepl("output = output", appServerText, fixed = TRUE))
  expect_true(grepl("modelFit = modelFit", appServerText, fixed = TRUE))
  expect_true(grepl("output$model_formula", formulaObserverText, fixed = TRUE))
  expect_true(grepl("withMathJax(HTML(formulaTex))", formulaObserverText, fixed = TRUE))
  expect_false(grepl("output$model_formula = renderUI", appServerText, fixed = TRUE))
})
