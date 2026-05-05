findPackageRootForReactiveStateTests = function(startDir = getwd()) {
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

readPackageTextForReactiveStateTests = function(...) {
  packageRoot = findPackageRootForReactiveStateTests()

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

testthat::test_that("app server reactive state is created through a helper", {
  appServerText = readPackageTextForReactiveStateTests("R", "app-server.R")
  reactiveStateText = readPackageTextForReactiveStateTests("R", "app-server-reactive-state.R")

  testthat::expect_true(grepl("createAppServerReactiveState", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("createAppServerReactiveState = function", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("rv = reactiveValues", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("modelFit = reactiveVal(NULL)", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("contrastPairs = reactiveVal(character(0))", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("contrastResultText = reactiveVal(\"\")", reactiveStateText, fixed = TRUE))
  testthat::expect_false(grepl("rv = reactiveValues", appServerText, fixed = TRUE))
})
