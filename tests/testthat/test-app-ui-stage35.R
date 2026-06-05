testthat::test_that("model tab places response transformation controls beside interaction controls", {
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("responseTransformationMode", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Response transformation handling", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-optional-controls-label", appUiText, fixed = TRUE))
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
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("wmfm-variable-bucket-header", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("gap: 18px", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("addDerivedVarBtn", modelSetupText, fixed = TRUE))
  testthat::expect_true(grepl("Add variable", modelSetupText, fixed = TRUE))
  testthat::expect_false(grepl("text = tags$div", modelSetupText, fixed = TRUE))
})

testthat::test_that("data context status is rendered beside the data context button", {
  modelHelpText = readPackageText("R", "app-server-model-help.R")

  testthat::expect_true(grepl("wmfm-data-context-inline-control", modelHelpText, fixed = TRUE))
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("Provided", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("Not provided", modelHelpText, fixed = TRUE))
  testthat::expect_true(grepl("wmfm-data-context-inline-control", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("flex-wrap: nowrap", appUiText, fixed = TRUE))

  inlineControlPosition = regexpr(
    "class = \"wmfm-data-context-inline-control\"",
    modelHelpText,
    fixed = TRUE
  )[[1]]
  inlineControlText = substring(modelHelpText, inlineControlPosition)
  buttonPosition = regexpr("id = \"modelHelpBtn\"", inlineControlText, fixed = TRUE)[[1]]
  statusPosition = regexpr("statusUi", inlineControlText, fixed = TRUE)[[1]]

  testthat::expect_true(inlineControlPosition > 0)
  testthat::expect_true(buttonPosition > 0)
  testthat::expect_true(statusPosition > buttonPosition)
})
