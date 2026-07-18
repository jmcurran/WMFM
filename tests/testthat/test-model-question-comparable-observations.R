testthat::test_that("good-deal questions route to comparable observations", {
  result = classifyModelFollowupQuestion("What is a good deal for a one carat diamond?")
  testthat::expect_identical(result$category, "comparable_observation_request")
  testthat::expect_true(result$supported)
})

testthat::test_that("comparable observations use model predictors deterministically", {
  data = mtcars
  data$cylGroup = factor(data$cyl)
  model = stats::lm(mpg ~ wt + cylGroup, data = data)
  payload = classifyModelFollowupQuestion(
    "What do similar cars look like for wt = 3 and cylGroup = 6?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(
    model,
    payload,
    neighbourCount = 4
  )
  result = payload$comparableObservationResult

  testthat::expect_identical(result$status, "ok")
  testthat::expect_equal(nrow(result$observations), 4)
  testthat::expect_true(all(diff(result$observations$distance) >= 0))
  testthat::expect_identical(result$predictorsUsed, c("wt", "cylGroup"))
})

testthat::test_that("comparable observations reject unsupported models", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  payload = classifyModelFollowupQuestion(
    "How does this car compare with similar cars for wt = 3?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(model, payload)
  testthat::expect_identical(
    payload$comparableObservationResult$status,
    "unsupported"
  )
})
