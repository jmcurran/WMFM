testthat::test_that("model summary output observers are registered from app server", {
  appServerText = readPackageText("R", "app-server.R")
  modelOutputText = readPackageText("R", "app-server-model-output.R")

  testthat::expect_true(grepl("registerModelSummaryObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelSummaryObservers = function", modelOutputText, fixed = TRUE))
  testthat::expect_true(grepl("output$model_output = renderPrint", modelOutputText, fixed = TRUE))
  testthat::expect_true(grepl("No model fitted yet.", modelOutputText, fixed = TRUE))
  testthat::expect_false(grepl("output$model_output = renderPrint", appServerText, fixed = TRUE))
})
