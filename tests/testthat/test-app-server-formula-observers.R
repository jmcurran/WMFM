test_that("model formula observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  formulaObserverText = readPackageText("R", "app-server-formula.R")

  expect_true(grepl("registerModelFormulaObservers", appServerText, fixed = TRUE))
  expect_true(grepl("registerModelFormulaObservers = function", formulaObserverText, fixed = TRUE))
  expect_true(grepl("output = output", appServerText, fixed = TRUE))
  expect_true(grepl("modelFit = modelFit", appServerText, fixed = TRUE))
  expect_true(grepl("output$model_formula", formulaObserverText, fixed = TRUE))
  expect_true(grepl("withMathJax(HTML(formulaTex))", formulaObserverText, fixed = TRUE))
  expect_false(grepl("output$model_formula = renderUI", appServerText, fixed = TRUE))
})
