fitModelObserverSourceFileAvailable = function(fileName) {
  candidateDirs = c(
    ".",
    "..",
    "../..",
    "../../.."
  )

  candidatePaths = unique(file.path(candidateDirs, "R", fileName))
  any(file.exists(candidatePaths))
}

findFitModelObserverSourceFile = function(fileName) {
  candidateDirs = c(
    ".",
    "..",
    "../..",
    "../../.."
  )

  candidatePaths = unique(file.path(candidateDirs, "R", fileName))
  existingPaths = candidatePaths[file.exists(candidatePaths)]

  if (length(existingPaths) == 0) {
    stop(
      paste0(
        "Could not locate ",
        fileName,
        " from testthat working directory"
      ),
      call. = FALSE
    )
  }

  normalizePath(existingPaths[1], winslash = "/", mustWork = TRUE)
}

readFitModelObserverSourceText = function(fileName) {
  path = findFitModelObserverSourceFile(fileName)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

skipIfFitModelObserverSourceFilesUnavailable = function() {
  if (!fitModelObserverSourceFileAvailable("app-server.R")) {
    testthat::skip(
      "Skipping source-inspection test because R/app-server.R is not available in this test environment"
    )
  }

  if (!fitModelObserverSourceFileAvailable("app-server-fit-model.R")) {
    testthat::skip(
      "Skipping source-inspection test because R/app-server-fit-model.R is not available in this test environment"
    )
  }
}

test_that("fit model observers are registered from app server", {
  skipIfFitModelObserverSourceFilesUnavailable()

  appServerText = readFitModelObserverSourceText("app-server.R")
  fitModelObserverText = readFitModelObserverSourceText("app-server-fit-model.R")

  expect_true(grepl("registerFitModelObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerFitModelObservers = function", fitModelObserverText, fixed = TRUE))
  expect_true(grepl("observeEvent(input$fit_btn", fitModelObserverText, fixed = TRUE))
  expect_true(grepl("output$formula_status", fitModelObserverText, fixed = TRUE))
  expect_true(grepl("observeEvent(input$reset_btn", fitModelObserverText, fixed = TRUE))
})
