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

test_that("contrast observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  contrastObserverText = readPackageText("R", "app-server-contrasts.R")

  expect_true(grepl("registerContrastObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerContrastObservers = function", contrastObserverText, fixed = TRUE))
  expect_true(grepl("rv = rv", appServerText, fixed = TRUE))
  expect_true(grepl("setBucketState = setBucketState", appServerText, fixed = TRUE))
  expect_true(grepl("output$contrasts_content_ui", contrastObserverText, fixed = TRUE))
  expect_true(grepl("output$contrastUi", contrastObserverText, fixed = TRUE))
  expect_true(grepl("output$contrastResult", contrastObserverText, fixed = TRUE))
  expect_false(grepl("output$contrastUi = renderUI", appServerText, fixed = TRUE))
})
