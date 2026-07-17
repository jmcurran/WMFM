testthat::test_that("good-deal questions are classified as conditional quantile requests", {
  questions = c(
    "What is a good deal for a one carat diamond?",
    "What is a relatively low price for a diamond with carat 1 and cut Ideal?",
    "What would count as a high mark for a student with attendance 80 and test score 70?",
    "What is the 25th percentile price for a one-carat diamond?"
  )

  for (question in questions) {
    result = classifyModelFollowupQuestion(question)

    testthat::expect_identical(result$category, "conditional_quantile_request")
    testthat::expect_false(result$supported)
    testthat::expect_identical(result$reason, "conditional_distribution_required")
    testthat::expect_match(result$deterministicResponse, "conditional mean", fixed = TRUE)
    testthat::expect_match(result$deterministicResponse, "quantile regression", fixed = TRUE)
  }
})

testthat::test_that("conditional quantile questions are not treated as predictions", {
  result = classifyModelFollowupQuestion(
    "What is a good deal for a one caret diamond?"
  )

  testthat::expect_identical(result$category, "conditional_quantile_request")
  testthat::expect_false(result$requiresDeterministicComputation)
  testthat::expect_null(result$predictionIntent)
})

testthat::test_that("existing-observation and coefficient questions are not captured", {
  questions = c(
    "Which diamonds have the lowest residuals?",
    "Which observations are cheap relative to their fitted values?",
    "Is the coefficient for carat high?",
    "Does a higher test score predict a higher exam mark?"
  )

  for (question in questions) {
    result = classifyModelFollowupQuestion(question)
    testthat::expect_false(identical(result$category, "conditional_quantile_request"))
  }
})

testthat::test_that("unsupported prompt block records the statistical reason", {
  payload = classifyModelFollowupQuestion(
    "What is a good deal for a one carat diamond?"
  )
  block = buildModelFollowupPromptBlock(followupPayload = payload)

  testthat::expect_match(block, "conditional_quantile_request", fixed = TRUE)
  testthat::expect_match(block, "unsupported for this pathway", fixed = TRUE)
  testthat::expect_false(grepl("one carat diamond", block, fixed = TRUE))
})
