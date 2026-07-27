testthat::test_that("Stage 49.2 builds an inspectable individual-prediction objective", {
  data = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72, 68, 77),
    Test = c(9, 13, 15, 19, 8, 13, 14, 16),
    Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)

  objective = buildResearchQuestionObjective(
    model,
    paste(
      "What exam mark would an individual student get if",
      "Attend = Yes and Test = 15?"
    )
  )

  testthat::expect_s3_class(objective, "wmfmQuestionObjective")
  testthat::expect_identical(objective$archetype, "individual_prediction")
  testthat::expect_false(objective$requiresFollowup)
  testthat::expect_identical(objective$profile$Attend, "Yes")
  testthat::expect_identical(objective$profile$Test, 15)
  testthat::expect_true("prediction interval" %in% objective$essentialConcepts)
})

testthat::test_that("Stage 49.2 records missing profile values instead of completing them silently", {
  data = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72),
    Test = c(9, 13, 15, 19, 8, 13),
    Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)

  objective = buildResearchQuestionObjective(
    model,
    "What exam mark would an individual student get for Test = 15?"
  )

  testthat::expect_identical(objective$archetype, "individual_prediction")
  testthat::expect_true(objective$requiresFollowup)
  testthat::expect_true("Attend" %in% objective$unsupportedOrMissing)
})

testthat::test_that("Stage 49.2 distinguishes expected response from individual prediction", {
  data = data.frame(Y = c(2, 3, 5, 6, 8, 9), X = 1:6)
  model = stats::lm(Y ~ X, data = data)

  objective = buildResearchQuestionObjective(
    model,
    "What is the expected response for X = 4?"
  )

  testthat::expect_identical(objective$archetype, "expected_response")
  testthat::expect_true("expected response" %in% objective$essentialConcepts)
})

testthat::test_that("Stage 49.2 characterises effect, comparison, uncertainty, and capability questions", {
  data = data.frame(Y = c(2, 3, 5, 6, 8, 9), X = 1:6)
  model = stats::lm(Y ~ X, data = data)

  cases = list(
    estimate_or_interpret_effect = "What is the effect of X on Y?",
    compare_groups_or_profiles = "How does group A compare with group B?",
    explain_uncertainty = "What does the confidence interval mean?",
    assess_model_capability = "Can this model answer my question?"
  )

  for (expected in names(cases)) {
    objective = buildResearchQuestionObjective(model, cases[[expected]])
    testthat::expect_identical(objective$archetype, expected)
  }
})

testthat::test_that("Stage 49.2 adds model-family concepts for logistic and Poisson questions", {
  logisticData = data.frame(
    Y = c(0, 1, 0, 1, 0, 1, 0, 1),
    X = c(1, 1, 2, 2, 3, 3, 4, 4)
  )
  logisticModel = stats::glm(Y ~ X, data = logisticData, family = stats::binomial())
  logisticObjective = buildResearchQuestionObjective(
    logisticModel,
    "Predict the outcome for an individual with X = 4."
  )

  poissonData = data.frame(Y = c(1, 2, 2, 3, 5, 7), X = 1:6)
  poissonModel = stats::glm(Y ~ X, data = poissonData, family = stats::poisson())
  poissonObjective = buildResearchQuestionObjective(
    poissonModel,
    "Predict the count for an individual with X = 4."
  )

  testthat::expect_true("predicted probability" %in% logisticObjective$essentialConcepts)
  testthat::expect_true("expected count" %in% poissonObjective$essentialConcepts)
  testthat::expect_false("prediction interval" %in% poissonObjective$essentialConcepts)
})

testthat::test_that("Stage 49.2 objective validation rejects incomplete contracts", {
  objective = structure(list(archetype = "individual_prediction"), class = c("wmfmQuestionObjective", "list"))
  testthat::expect_error(
    validateWmfmQuestionObjective(objective),
    "missing required fields",
    fixed = TRUE
  )
})


testthat::test_that("Stage 49.2 recognises the Course do-well question as prediction-shaped", {
  data = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72),
    Test = c(9, 13, 15, 19, 8, 13),
    Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)

  objective = buildResearchQuestionObjective(
    model,
    paste(
      "Will I do well on the final exam if I attend class regularly",
      "and get a good mark in the test?"
    )
  )

  testthat::expect_identical(objective$archetype, "individual_prediction")
  testthat::expect_true(objective$requiresFollowup)
  testthat::expect_true(all(c("Attend", "Test", "outcome_threshold") %in% objective$unsupportedOrMissing))
})
