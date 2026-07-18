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


testthat::test_that("comparable-observation prompt supplies deterministic cases and guardrails", {
  data = mtcars
  data$cylGroup = factor(data$cyl)
  model = stats::lm(mpg ~ wt + cylGroup, data = data)
  payload = classifyModelFollowupQuestion(
    "What is a good deal for wt = 3 and cylGroup = 6?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(
    model,
    payload,
    neighbourCount = 4L
  )
  block = buildModelFollowupPromptBlock(followupPayload = payload)

  testthat::expect_match(block, "WMFM deterministic comparable-observation payload", fixed = TRUE)
  testthat::expect_match(block, "Rank 1", fixed = TRUE)
  testthat::expect_match(block, "will render the complete comparable-case answer separately", fixed = TRUE)
  testthat::expect_match(block, "Do not declare a bargain", fixed = TRUE)
})

testthat::test_that("deterministic comparable answers report verified nearest cases", {
  data = mtcars
  data$cylGroup = factor(data$cyl)
  model = stats::lm(mpg ~ wt + cylGroup, data = data)
  payload = classifyModelFollowupQuestion(
    "What do similar cars look like for wt = 3 and cylGroup = 6?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(
    model,
    payload,
    neighbourCount = 4L
  )
  attr(model, "wmfm_model_followup_payload") = payload

  answer = buildDeterministicFollowupAnswer(model)
  observations = payload$comparableObservationResult$observations

  testthat::expect_match(answer, "nearest observations", fixed = TRUE)
  testthat::expect_match(answer, observations$observation[[1]], fixed = TRUE)
  testthat::expect_match(
    answer,
    formatFollowupPredictionNumber(observations$response[[1]]),
    fixed = TRUE
  )
  testthat::expect_match(answer, "with a median of", fixed = TRUE)
  testthat::expect_match(answer, "do not by themselves establish", fixed = TRUE)
})

testthat::test_that("deterministic comparable answers append once", {
  data = mtcars
  data$cylGroup = factor(data$cyl)
  model = stats::lm(mpg ~ wt + cylGroup, data = data)
  payload = classifyModelFollowupQuestion(
    "What do similar cars look like for wt = 3 and cylGroup = 6?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(
    model,
    payload,
    neighbourCount = 4L
  )
  attr(model, "wmfm_model_followup_payload") = payload

  once = appendDeterministicFollowupAnswer("Main explanation.", model)
  twice = appendDeterministicFollowupAnswer(once, model)

  testthat::expect_identical(twice, once)
  testthat::expect_match(once, "Main explanation.", fixed = TRUE)
  testthat::expect_match(once, "nearest observations", fixed = TRUE)
})

testthat::test_that("unsupported comparable answers fail without invented cases", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  payload = classifyModelFollowupQuestion(
    "How does this car compare with similar cars for wt = 3?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  answer = buildDeterministicFollowupAnswer(model)

  testthat::expect_match(answer, "could not compute comparable observations", fixed = TRUE)
  testthat::expect_match(answer, "ordinary linear models only", fixed = TRUE)
  testthat::expect_match(answer, "has not invented", fixed = TRUE)
  testthat::expect_false(grepl("nearest observations among", answer, fixed = TRUE))
})
