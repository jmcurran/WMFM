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

test_that("data loading observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  dataObserverText = readPackageText("R", "app-server-data-observers.R")

  expect_true(grepl("registerDataLoadObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerDataLoadObservers = function", dataObserverText, fixed = TRUE))
  expect_true(grepl("exampleLoadStatus = exampleLoadStatus", appServerText, fixed = TRUE))
  expect_true(grepl("resetModelPage = resetModelPage", appServerText, fixed = TRUE))
  expect_true(grepl("applyLoadedExampleToInputs = applyLoadedExampleToInputs", appServerText, fixed = TRUE))
  expect_true(grepl("observeEvent(input$file", dataObserverText, fixed = TRUE))
  expect_true(grepl("observeEvent(input$confirm_sep", dataObserverText, fixed = TRUE))
  expect_true(grepl("observeEvent(input$package_dataset", dataObserverText, fixed = TRUE))
  expect_true(grepl("observeEvent(input$loadExampleBtn", dataObserverText, fixed = TRUE))
  expect_false(grepl("loadDelimited = function", appServerText, fixed = TRUE))
  expect_false(grepl("observeEvent(input$file", appServerText, fixed = TRUE))
  expect_false(grepl("observeEvent(input$package_dataset", appServerText, fixed = TRUE))
})
