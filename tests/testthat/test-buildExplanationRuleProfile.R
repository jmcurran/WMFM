test_that("buildExplanationRuleProfile creates intercept-only rules", {
  profile = list(
    modelFamily = "lm",
    modelStructure = "interceptOnly",
    modelScale = "response",
    interpretationScale = "response",
    transformationType = "none",
    predictorTypes = list(numeric = character(0), factor = character(0), other = character(0)),
    hasInteractions = FALSE,
    comparisonScope = "none"
  )

  out = buildExplanationRuleProfile(profile)

  expect_identical(out$skeletonId, "lm_interceptOnly")
  expect_identical(out$comparisonScope, "none")
  expect_identical(
    out$skeletonSteps$stepRole,
    c("estimate", "uncertainty", "answer")
  )
  expect_false("modelConstraint" %in% out$skeletonSteps$stepRole)
  expect_match(
    out$skeletonSteps$instruction[out$skeletonSteps$stepRole == "uncertainty"],
    "underlying average",
    fixed = TRUE
  )
  expect_true("interceptOnlyMentionsPredictors" %in% out$qualityFlagsToCheck)
  expect_true("lmInterceptOnlyUsesRSquared" %in% out$qualityFlagsToCheck)
})

test_that("buildExplanationRuleProfile creates numeric-predictor change guidance", {
  profile = list(
    modelFamily = "lm",
    modelStructure = "numericMainEffect",
    modelScale = "response",
    interpretationScale = "response",
    transformationType = "none",
    predictorTypes = list(numeric = "x", factor = character(0), other = character(0)),
    hasInteractions = FALSE,
    comparisonScope = "minimal"
  )

  out = buildExplanationRuleProfile(profile)

  expect_identical(out$skeletonId, "lm_numericMainEffect")
  expect_true("effect" %in% out$skeletonSteps$stepRole)
  expect_match(out$effectLanguage, "change", fixed = TRUE)
  expect_true("effectWithoutChangeLanguage" %in% out$qualityFlagsToCheck)
})

test_that("buildExplanationRuleProfile limits factor comparisons", {
  profile = list(
    modelFamily = "lm",
    modelStructure = "additive",
    modelScale = "response",
    interpretationScale = "response",
    transformationType = "none",
    predictorTypes = list(numeric = character(0), factor = c("a", "b", "c"), other = character(0)),
    hasInteractions = FALSE,
    comparisonScope = "minimal"
  )

  out = buildExplanationRuleProfile(profile)

  expect_identical(out$comparisonScope, "minimal")
  expect_match(out$comparisonGuidance, "without enumerating", fixed = TRUE)
  expect_true("comparisonExplosion" %in% out$qualityFlagsToCheck)
  expect_true("excessiveComparisons" %in% out$qualityFlagsToCheck)
})

test_that("buildExplanationRuleProfile creates interaction structure", {
  profile = list(
    modelFamily = "poisson",
    modelStructure = "interaction",
    modelScale = "logExpectedCount",
    interpretationScale = "expectedCount",
    transformationType = "none",
    predictorTypes = list(numeric = "magnitude", factor = "locn", other = character(0)),
    hasInteractions = TRUE,
    comparisonScope = "targeted"
  )

  out = buildExplanationRuleProfile(profile)

  expect_identical(out$skeletonId, "poisson_interaction")
  expect_identical(
    out$skeletonSteps$stepRole[1:3],
    c("firstWithinGroupEffect", "secondWithinGroupEffect", "effectComparison")
  )
  expect_match(out$effectLanguage, "within each relevant group", fixed = TRUE)
  expect_match(
    out$skeletonSteps$instruction[out$skeletonSteps$stepRole == "effectComparison"],
    "Compare the within-group effects directly",
    fixed = TRUE
  )
  expect_true("interactionTermMentioned" %in% out$qualityFlagsToCheck)
  expect_true("interaction term" %in% out$avoidTerms)
  expect_match(out$scaleGuidance, "expected counts", fixed = TRUE)
})

test_that("buildExplanationRuleProfile records logistic scale guidance", {
  profile = list(
    modelFamily = "logistic",
    modelStructure = "factorMainEffect",
    modelScale = "logOdds",
    interpretationScale = "probability",
    transformationType = "none",
    predictorTypes = list(numeric = character(0), factor = "group", other = character(0)),
    hasInteractions = FALSE,
    comparisonScope = "minimal"
  )

  out = buildExplanationRuleProfile(profile)

  expect_match(out$scaleGuidance, "probabilities", fixed = TRUE)
  expect_true("logisticMentionsLogOdds" %in% out$qualityFlagsToCheck)
  expect_true("log-odds" %in% out$avoidTerms)
})

test_that("buildExplanationRuleProfile records transformed-response guidance", {
  profile = list(
    modelFamily = "lm",
    modelStructure = "numericMainEffect",
    modelScale = "log(y)",
    interpretationScale = "originalResponse",
    transformationType = "log",
    predictorTypes = list(numeric = "x", factor = character(0), other = character(0)),
    hasInteractions = FALSE,
    comparisonScope = "minimal"
  )

  out = buildExplanationRuleProfile(profile)

  expect_identical(out$modelScale, "log(y)")
  expect_identical(out$interpretationScale, "originalResponse")
  expect_match(out$scaleGuidance, "student-facing explanations", fixed = TRUE)
})

test_that("buildExplanationRuleProfile validates required profile fields", {
  expect_error(
    buildExplanationRuleProfile(list(modelFamily = "lm")),
    "missing required fields",
    fixed = TRUE
  )
})
