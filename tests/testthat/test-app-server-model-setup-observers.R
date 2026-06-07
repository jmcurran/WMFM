testthat::test_that("model setup observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  modelSetupText = readPackageText("R", "app-server-model-setup.R")

  testthat::expect_true(grepl("registerModelSetupObservers", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("registerModelSetupObservers = function", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("setBucketState = setBucketState", appServerText, fixed = TRUE))
  testthat::expect_true(grepl("output$var_buckets = renderUI", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("output$interaction_ui = renderUI", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$factors", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("observeEvent(input$confirmAddDerivedVarBtn", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("output$response_picker = renderUI", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("updateTextInput(session, \"formula_text\"", modelSetupText, fixed = TRUE))
  testthat::expect_false(grepl("output$var_buckets = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("output$response_picker = renderUI", appServerText, fixed = TRUE))
  testthat::expect_false(grepl("observeEvent(input$confirmAddDerivedVarBtn", appServerText, fixed = TRUE))
})

testthat::test_that("response changes resynchronise derived response formulas", {
  modelSetupText = readPackageText("R", "app-server-model-setup.R")

  testthat::expect_true(grepl("observeEvent(input$response_var", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("derivedResponseFormula = substituteDerivedResponseInFormula", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("updateTextInput(session, \"formula_text\", value = derivedResponseFormula)", modelSetupText, fixed = TRUE))
})
