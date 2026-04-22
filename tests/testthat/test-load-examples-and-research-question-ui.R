sourceFileAvailableForExampleTests = function(fileName) {
  candidateDirs = c(
    ".",
    "..",
    "../..",
    "../../.."
  )

  candidatePaths = unique(file.path(candidateDirs, "R", fileName))
  any(file.exists(candidatePaths))
}

findProjectFileForExampleTests = function(fileName) {
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
      )
    )
  }

  normalizePath(existingPaths[1], winslash = "/", mustWork = TRUE)
}

readProjectFileTextForExampleTests = function(fileName) {
  path = findProjectFileForExampleTests(fileName)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

skipIfExampleSourceFilesUnavailable = function() {
  if (!sourceFileAvailableForExampleTests("app-server.R")) {
    testthat::skip(
      "Skipping source-inspection test because R/app-server.R is not available in this test environment"
    )
  }

  if (!sourceFileAvailableForExampleTests("app-ui.R")) {
    testthat::skip(
      "Skipping source-inspection test because R/app-ui.R is not available in this test environment"
    )
  }
}

test_that("example-loading source files are present", {
  skipIfExampleSourceFilesUnavailable()

  expect_true(file.exists(findProjectFileForExampleTests("app-server.R")))
  expect_true(file.exists(findProjectFileForExampleTests("app-ui.R")))
})

test_that("app server keeps the guarded bucket-sync logic", {
  skipIfExampleSourceFilesUnavailable()

  serverText = readProjectFileTextForExampleTests("app-server.R")

  expect_true(grepl(
    "cur = input$factors %||% character(0)",
    serverText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "cur = input$continuous %||% character(0)",
    serverText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "setBucketState(",
    serverText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "rv$bucketFactors",
    serverText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "rv$bucketContinuous",
    serverText,
    fixed = TRUE
  ))
})

test_that("app server requires a research question before fitting", {
  skipIfExampleSourceFilesUnavailable()

  serverText = readProjectFileTextForExampleTests("app-server.R")

  expect_true(grepl(
    'trimws(input$researchQuestion %||% "")',
    serverText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "Please enter the research question before fitting the model.",
    serverText,
    fixed = TRUE
  ))
})

test_that("app server keeps pending example interactions available", {
  skipIfExampleSourceFilesUnavailable()

  serverText = readProjectFileTextForExampleTests("app-server.R")

  expect_true(grepl(
    "pendingExampleInteractions = character(0)",
    serverText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "rv$pendingExampleInteractions = interactionTerms",
    serverText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "input$interactions",
    serverText,
    fixed = TRUE
  ))
})
