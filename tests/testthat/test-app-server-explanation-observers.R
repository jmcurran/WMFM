testthat::test_that("model explanation observers are registered from app server", {
  appServerText = readPackageText("R", "app-server.R")
  explanationText = readPackageText("R", "app-server-explanation.R")

  testthat::expect_true(grepl("registerModelExplanationObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelExplanationObservers = function", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("output$model_explanation = renderUI", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$modelExplanationTutorBtn", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("output$developerFeedbackReportDownload", explanationText, fixed = TRUE))
  testthat::expect_true(grepl("modelExplanationTeachingSummary", explanationText, fixed = TRUE))
  testthat::expect_false(grepl("output$model_explanation = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("observeEvent(input$modelExplanationTutorBtn", appServerText, fixed = TRUE))
})
