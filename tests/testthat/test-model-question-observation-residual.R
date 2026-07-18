testthat::test_that("existing-observation residual questions are classified by direction", {
  questions = list(
    lower = c(
      "Which diamonds have the lowest residuals?",
      "Which observations are furthest below their fitted values?",
      "Find the students who performed worse than expected."
    ),
    higher = c(
      "Which students have the highest residuals?",
      "Show the observations above their fitted values.",
      "Which students performed better than expected?"
    ),
    absolute = c(
      "Which observations have the largest absolute residuals?",
      "Identify the cases furthest from their fitted values.",
      "Which points are most unusual relative to the model?"
    )
  )

  for (direction in names(questions)) {
    for (question in questions[[direction]]) {
      result = classifyModelFollowupQuestion(question)
      testthat::expect_identical(result$category, "observation_residual_request")
      testthat::expect_true(result$supported)
      testthat::expect_true(result$requiresDeterministicComputation)
      testthat::expect_identical(result$observationDirection, direction)
    }
  }
})

testthat::test_that("conditional-value and broad coefficient questions remain separate", {
  questions = c(
    "What is a good deal for a one carat diamond?",
    "Is the coefficient for carat high?",
    "Does a higher test score predict a higher exam mark?",
    "What is a cheap price for a diamond with carat 1?"
  )

  categories = vapply(
    questions,
    function(question) classifyModelFollowupQuestion(question)$category,
    character(1)
  )

  testthat::expect_identical(categories[[1]], "conditional_quantile_request")
  testthat::expect_false(any(categories == "observation_residual_request"))
})

testthat::test_that("ordinary lm residual rankings use observed minus fitted", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  payload = classifyModelFollowupQuestion(
    "Which cars have the most negative residuals?"
  )
  payload = enrichFollowupPayloadWithObservationResiduals(model, payload, observationCount = 3L)
  result = payload$observationResidualResult

  testthat::expect_identical(result$status, "ok")
  testthat::expect_identical(result$direction, "lower")
  testthat::expect_identical(result$rankingMetric, "raw_residual")
  testthat::expect_equal(result$observations$residual, sort(stats::residuals(model))[1:3])
  testthat::expect_equal(
    unname(result$observations$residual),
    unname(result$observations$observed - result$observations$fitted)
  )
  testthat::expect_true(all(result$observations$observation %in% row.names(mtcars)))
})

testthat::test_that("absolute residual ranking is stable and bounded", {
  data = data.frame(y = c(0, 2, 4, 8), x = c(0, 1, 2, 3))
  model = stats::lm(y ~ x, data = data)
  result = computeObservationResidualResult(
    model = model,
    direction = "absolute",
    observationCount = 2L
  )

  testthat::expect_identical(result$status, "ok")
  testthat::expect_equal(nrow(result$observations), 2L)
  testthat::expect_true(all(diff(abs(result$observations$residual)) <= 0))
  testthat::expect_true(all(result$observations$percentile > 0))
  testthat::expect_true(all(result$observations$percentile < 100))
})

testthat::test_that("GLM residual inspection reports the current scope boundary", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  result = computeObservationResidualResult(model, direction = "higher")

  testthat::expect_identical(result$status, "unsupported")
  testthat::expect_identical(result$reason, "ordinary_lm_required")
})

testthat::test_that("residual prompt block supplies values and guardrails", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  payload = classifyModelFollowupQuestion(
    "Which cars have the largest absolute residuals?"
  )
  payload = enrichFollowupPayloadWithObservationResiduals(model, payload, observationCount = 2L)
  block = buildModelFollowupPromptBlock(followupPayload = payload)

  testthat::expect_match(block, "WMFM deterministic existing-observation residual payload", fixed = TRUE)
  testthat::expect_match(block, "observed minus fitted", fixed = TRUE)
  testthat::expect_match(block, "Rank 1", fixed = TRUE)
  testthat::expect_match(block, "Do not call an observation a bargain", fixed = TRUE)
  testthat::expect_false(grepl("conditional percentile", block, fixed = TRUE) && grepl("calculate", block, fixed = TRUE))
})
