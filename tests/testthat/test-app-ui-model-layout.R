testthat::test_that("model tab contains response transformation and interaction controls", {
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("responseTransformationMode", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Response transformation", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-optional-controls-row", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-optional-controls-label", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-optional-info-icon", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Both", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Model scale", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Original response scale", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Choose how explanations should use an invertible transformation", appUiText, fixed = TRUE))

  testthat::expect_true(grepl("interaction_label_ui", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("interaction_ui", appUiText, fixed = TRUE))
})

testthat::test_that("variable bucket headers use a shared aligned structure", {
  modelSetupText = readPackageText("R", "app-server-model-setup.R")
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("wmfm-variable-bucket-header", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-variable-bucket-title", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("addDerivedVarBtn", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("Add variable", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("Factors</span>", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("Numeric</span>", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("align-items: center", appUiText, fixed = TRUE))
})

testthat::test_that("data context status is rendered beside the data context button", {
  modelHelpText = readPackageText("R", "app-server-model-help.R")
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("wmfm-data-context-inline-control", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("Provided", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("Not provided", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-data-context-inline-control", appUiText, fixed = TRUE))

  inlineControlPosition = regexpr(
    "wmfm-data-context-inline-control",
    modelHelpText,
    fixed = TRUE
  )[[1]]
  inlineControlText = substring(modelHelpText, inlineControlPosition)
  buttonPosition = regexpr("modelHelpBtn", inlineControlText, fixed = TRUE)[[1]]
  statusPosition = regexpr("statusUi", inlineControlText, fixed = TRUE)[[1]]

  testthat::expect_true(inlineControlPosition > 0)
  testthat::expect_true(buttonPosition > 0)
  testthat::expect_true(statusPosition > buttonPosition)
})
