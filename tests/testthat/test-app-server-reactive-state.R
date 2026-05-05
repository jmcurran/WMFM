testthat::test_that("app server reactive state is created through a helper", {
  appServerText = readPackageText("R", "app-server.R")
  reactiveStateText = readPackageText("R", "app-server-reactive-state.R")

  testthat::expect_true(grepl("createAppServerReactiveState", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("createAppServerReactiveState = function", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("rv = reactiveValues", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("modelFit = reactiveVal(NULL)", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("contrastPairs = reactiveVal(character(0))", reactiveStateText, fixed = TRUE))
  testthat::expect_true(grepl("contrastResultText = reactiveVal(\"\")", reactiveStateText, fixed = TRUE))
  testthat::expect_false(grepl("rv = reactiveValues", appServerText, fixed = TRUE))
})
