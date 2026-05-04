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

test_that("developer mode source controls are password protected", {
  skipIfExampleSourceFilesUnavailable()

  uiText = readProjectFileTextForExampleTests("app-ui.R")
  serverText = readProjectFileTextForExampleTests("app-server.R")
  startupObserverText = readProjectFileTextForExampleTests("app-server-startup.R")
  developerModeHelperText = readProjectFileTextForExampleTests("app-server-developer-mode-helpers.R")
  developerModeAuthText = readProjectFileTextForExampleTests("utils-developerModeAuth.R")
  developerModeText = paste(
    serverText,
    startupObserverText,
    developerModeHelperText,
    developerModeAuthText,
    sep = "\n"
  )

  expect_true(grepl(
    'inputId = "developerModePassword"',
    uiText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "Unlock developer mode",
    uiText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "verifyDeveloperModePassword",
    developerModeText,
    fixed = TRUE
  ))

  expect_true(grepl(
    "includeTestExamples = isTRUE(developerModeUnlocked())",
    developerModeText,
    fixed = TRUE
  ))
})

test_that("packaged test examples are hidden unless explicitly requested", {
  visibleExamples = listWMFMExamples(package = "WMFM")
  developerExamples = listWMFMExamples(
    package = "WMFM",
    includeTestExamples = TRUE
  )

  expect_false(any(startsWith(visibleExamples, "test")))
  expect_true(any(startsWith(developerExamples, "test")))
  expect_true(all(visibleExamples %in% developerExamples))
})

test_that("developer test example ladder contains the expected model families", {
  developerExamples = listWMFMExamples(
    package = "WMFM",
    includeTestExamples = TRUE
  )

  expectedExamples = c(
    "test-01-G00F",
    "test-02-G01F",
    "test-03-G10F",
    "test-04-G20F",
    "test-05-G20T",
    "test-06-G11F",
    "test-07-G11T",
    "test-08-B00F",
    "test-09-B01F",
    "test-10-B10F",
    "test-11-B20F",
    "test-12-B20T",
    "test-13-B11F",
    "test-14-B11T",
    "test-15-P00F",
    "test-16-P01F",
    "test-17-P10F",
    "test-18-P11F",
    "test-19-P11T"
  )

  expect_true(all(expectedExamples %in% developerExamples))
})
