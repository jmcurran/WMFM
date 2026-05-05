findPackageRootForObserverDependencyTests = function(startDir = getwd()) {
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

readPackageTextForObserverDependencyTests = function(...) {
  packageRoot = findPackageRootForObserverDependencyTests()

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

testthat::test_that("app server observer dependencies are created through a helper", {
  appServerText = readPackageTextForObserverDependencyTests("R", "app-server.R")
  dependencyText = readPackageTextForObserverDependencyTests("R", "app-server-observer-dependencies.R")

  testthat::expect_true(grepl("createAppServerObserverDependencies", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("createAppServerObserverDependencies = function", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("createAppServerStateHelpers", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("setBucketState = serverStateHelpers$setBucketState", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("resetModelPage = serverStateHelpers$resetModelPage", dependencyText, fixed = TRUE))
  testthat::expect_true(grepl("applyLoadedExampleToInputs = serverStateHelpers$applyLoadedExampleToInputs", dependencyText, fixed = TRUE))
  testthat::expect_false(grepl("serverStateHelpers = createAppServerStateHelpers", appServerText, fixed = TRUE))
})
