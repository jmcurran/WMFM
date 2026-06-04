testthat::test_that("model tab places response transformation controls beside interaction controls", {
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("responseTransformationMode", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Response transformation handling", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Both", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Model scale", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Original response scale", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Choose how explanations should use an invertible transformation", appUiText, fixed = TRUE))

  responseModePosition = regexpr("responseTransformationMode", appUiText, fixed = TRUE)[[1]]
  interactionLabelPosition = regexpr("interaction_label_ui", appUiText, fixed = TRUE)[[1]]
  interactionUiPosition = regexpr("interaction_ui", appUiText, fixed = TRUE)[[1]]

  testthat::expect_true(responseModePosition > 0)
  testthat::expect_true(interactionLabelPosition > responseModePosition)
  testthat::expect_true(interactionUiPosition > interactionLabelPosition)
})

testthat::test_that("variables bucket header contains the add-variable button", {
  modelSetupText = readPackageText("R", "app-server-model-setup.R")

  testthat::expect_true(grepl("wmfm-variable-bucket-header", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("addDerivedVarBtn", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl('"Add variable"', modelSetupText, fixed = TRUE))
})

testthat::test_that("data context status is rendered beside the data context button", {
  modelHelpText = readPackageText("R", "app-server-model-help.R")

  testthat::expect_true(grepl("wmfm-data-context-inline-control", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("Data context provided.", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("No data context provided yet.", modelHelpText, fixed = TRUE))
})
