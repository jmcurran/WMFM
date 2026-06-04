testthat::test_that("model tab places derived-variable controls with interaction controls", {
  appUiText = readPackageText("R", "app-ui.R")

  testthat::expect_true(grepl("addDerivedVarBtn", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("responseTransformationMode", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Response transformation handling:", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Both model scale and original response scale", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Model scale only", appUiText, fixed = TRUE))
  testthat::expect_true(grepl("Original response scale when available", appUiText, fixed = TRUE))

  derivedButtonPosition = regexpr("addDerivedVarBtn", appUiText, fixed = TRUE)[[1]]
  interactionLabelPosition = regexpr("interaction_label_ui", appUiText, fixed = TRUE)[[1]]
  responseModePosition = regexpr("responseTransformationMode", appUiText, fixed = TRUE)[[1]]
  interactionUiPosition = regexpr("interaction_ui", appUiText, fixed = TRUE)[[1]]

  testthat::expect_true(derivedButtonPosition > 0)
  testthat::expect_true(interactionLabelPosition > 0)
  testthat::expect_true(responseModePosition > derivedButtonPosition)
  testthat::expect_true(interactionUiPosition > responseModePosition)
})
