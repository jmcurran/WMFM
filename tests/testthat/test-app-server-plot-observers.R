test_that("model plot observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  plotObserverText = readPackageText("R", "app-server-plot.R")

  expect_true(grepl("registerModelPlotObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerModelPlotObservers = function", plotObserverText, fixed = TRUE))
  expect_true(grepl("modelFit = modelFit", appServerText, fixed = TRUE))
  expect_true(grepl("output$plot_ci_controls_ui", plotObserverText, fixed = TRUE))
  expect_true(grepl("output$model_plot", plotObserverText, fixed = TRUE))
  expect_true(grepl("drawModelPlot", plotObserverText, fixed = TRUE))
  expect_false(grepl("output$plot_ci_controls_ui = renderUI", appServerText, fixed = TRUE))
  expect_false(grepl("output$model_plot = renderPlot", appServerText, fixed = TRUE))
})
