testthat::test_that("model help observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  modelHelpText = readPackageText("R", "app-server-model-help.R")

  testthat::expect_true(grepl("registerModelHelpObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelHelpObservers = function", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("rv = rv", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("output$modelHelpBtnUi", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("output$userDatasetContextUi", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("output$modelHelpModalBody", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$modelHelpBtn", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$saveUserDatasetContextBtn", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$researchQuestion", modelHelpText, fixed = TRUE))
  testthat::expect_false(grepl("output$modelHelpBtnUi = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("output$modelHelpModalBody = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("observeEvent(input$modelHelpBtn", appServerText, fixed = TRUE))
})


testthat::test_that("model help UI includes data context status-aware button markup", {
  modelHelpText = readPackageText("R", "app-server-model-help.R")

  testthat::expect_true(grepl("btn btn-danger action-button", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("btn btn-success action-button", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("Edit data context", modelHelpText, fixed = TRUE))
  testthat::expect_false(grepl("Data description", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("Provided", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("Not provided", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-formula-status wmfm-formula-status-ok", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-formula-status wmfm-formula-status-error", modelHelpText, fixed = TRUE))
})
