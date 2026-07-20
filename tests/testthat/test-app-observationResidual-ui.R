testthat::test_that("residual display table preserves verified values", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  result = computeObservationResidualResult(
    model = model,
    direction = "lower",
    observationCount = 3L
  )

  table = buildObservationResidualDisplayTable(result)

  testthat::expect_identical(nrow(table), 3L)
  testthat::expect_identical(
    names(table),
    c(
      "Rank", "Observation", "Source row", "Observed", "Fitted",
      "Residual", "Ranking percentile"
    )
  )
  testthat::expect_equal(table$Residual, signif(unname(result$observations$residual), 6))
  testthat::expect_identical(table$Observation, result$observations$observation)
})

testthat::test_that("residual result UI presents a bounded deterministic table", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  payload = classifyModelFollowupQuestion(
    "Which cars have the most negative residuals?"
  )
  payload = enrichFollowupPayloadWithObservationResiduals(
    model,
    payload,
    observationCount = 2L
  )

  html = htmltools::renderTags(
    buildObservationResidualResultUi(payload)
  )$html

  testthat::expect_match(html, "Existing observations compared with fitted values", fixed = TRUE)
  testthat::expect_match(html, "Observed", fixed = TRUE)
  testthat::expect_match(html, "Fitted", fixed = TRUE)
  testthat::expect_match(html, "Residual", fixed = TRUE)
  testthat::expect_match(html, payload$observationResidualResult$observations$observation[[1]], fixed = TRUE)
  testthat::expect_match(html, "do not by themselves identify bargains", fixed = TRUE)
})

testthat::test_that("residual result UI shows the model scope boundary", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  payload = classifyModelFollowupQuestion(
    "Which cars have the highest residuals?"
  )
  payload = enrichFollowupPayloadWithObservationResiduals(model, payload)

  html = htmltools::renderTags(
    buildObservationResidualResultUi(payload)
  )$html

  testthat::expect_match(html, "Observation comparison unavailable", fixed = TRUE)
  testthat::expect_match(html, "ordinary linear models only", fixed = TRUE)
  testthat::expect_false(grepl("<table", html, fixed = TRUE))
})

testthat::test_that("residual result UI is absent for other follow-up categories", {
  payload = classifyModelFollowupQuestion(
    "What is the expected response when wt is 3?"
  )

  testthat::expect_null(buildObservationResidualResultUi(payload))
})
