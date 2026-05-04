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

test_that("app server state helpers are extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  stateHelperText = readPackageText("R", "app-server-state-helpers.R")

  expect_match(appServerText, "createAppServerStateHelpers", fixed = TRUE)
  expect_false(grepl("setBucketState = function", appServerText, fixed = TRUE))
  expect_false(grepl("resetModelPage = function", appServerText, fixed = TRUE))
  expect_false(grepl("applyLoadedExampleToInputs = function", appServerText, fixed = TRUE))

  expect_match(stateHelperText, "createAppServerStateHelpers = function", fixed = TRUE)
  expect_match(stateHelperText, "setBucketState = function", fixed = TRUE)
  expect_match(stateHelperText, "resetModelPage = function", fixed = TRUE)
  expect_match(stateHelperText, "applyLoadedExampleToInputs = function", fixed = TRUE)
})
