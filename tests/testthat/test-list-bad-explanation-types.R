test_that("listBadExplanationTypes returns the locked v1 type set", {

  out = listBadExplanationTypes()

  expect_type(out, "character")
  expect_false(anyDuplicated(out))

  expect_identical(
    out,
    c(
      "nullAlternativeConfusion",
      "nullAcceptanceError",
      "causalInferenceError",
      "effectDirectionError",
      "wrongScaleError",
      "referenceLevelError",
      "factorOffsetOmissionError",
      "interactionIgnoredError",
      "mainEffectOverinterpretationWithInteraction",
      "confidenceIntervalProofLanguage",
      "rSquaredOverclaim",
      "inferenceOmission",
      "logicalContradiction"
    )
  )
})
