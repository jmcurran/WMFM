test_that("numeric factor confirmation helpers preserve modal wording", {
  expect_identical(buildNumericFactorConfirmTitle(), "Treat numeric variable as factor?")
  expect_match(buildNumericFactorConfirmMessage("age"), "You moved 'age' into the Factors bucket", fixed = TRUE)
  expect_match(buildNumericFactorConfirmMessage("age"), "Do you want to treat this variable as a factor in the model?", fixed = TRUE)
  expect_identical(buildNumericFactorCancelLabel(), "No, I'll move it back")
  expect_identical(buildNumericFactorConfirmLabel(), "Yes, treat as factor")
})

test_that("derived variable helpers preserve modal wording", {
  expect_identical(buildAddDerivedVariableTitle(), "Add derived variable")
  expect_match(buildAddDerivedVariableHelpText(), "Enter a single R expression of the form newVariable = ...", fixed = TRUE)
  expect_match(buildAddDerivedVariableHelpText(), "available in the response picker and variable buckets", fixed = TRUE)
  expect_match(buildAddDerivedVariablePlaceholder(), "t = 1:nrow(data)", fixed = TRUE)
  expect_match(buildAddDerivedVariablePlaceholder(), "month = factor(rep(1:12, 12))", fixed = TRUE)
  expect_identical(buildAddDerivedVariableConfirmLabel(), "Add variable")
})
