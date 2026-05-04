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

test_that("startup observer registration is extracted from app server", {
  serverText = readPackageText("R", "app-server.R")
  startupObserverText = readPackageText("R", "app-server-startup.R")

  expect_match(serverText, "registerStartupDataChoiceObservers", fixed = TRUE)
  expect_match(startupObserverText, "registerStartupDataChoiceObservers = function", fixed = TRUE)
  expect_match(startupObserverText, "developerModeUnlocked = reactiveVal", fixed = TRUE)
  expect_match(startupObserverText, "exampleLoadStatus = reactiveVal", fixed = TRUE)
  expect_false(grepl("startupNotificationId = \"wmfm-startup-data-choices\"", serverText, fixed = TRUE))
})
