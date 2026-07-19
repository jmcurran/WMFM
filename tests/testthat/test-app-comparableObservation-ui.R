testthat::test_that("comparable display table preserves verified values", {
  data = mtcars
  data$cylGroup = factor(data$cyl)
  model = stats::lm(mpg ~ wt + cylGroup, data = data)
  result = computeComparableObservationResult(
    model = model,
    followupQuestion = "What do similar cars look like for wt = 3 and cylGroup = 6?",
    neighbourCount = 8L
  )

  table = buildComparableObservationDisplayTable(result, displayCount = 5L)

  testthat::expect_identical(nrow(table), 5L)
  testthat::expect_identical(
    names(table),
    c("Rank", "Observation", "Source row", "Observed mpg", "Similarity distance")
  )
  testthat::expect_equal(table$`Observed mpg`, signif(result$observations$response[1:5], 6))
  testthat::expect_equal(table$`Similarity distance`, signif(result$observations$distance[1:5], 6))
})

testthat::test_that("comparable result UI presents a bounded deterministic table", {
  data = mtcars
  data$cylGroup = factor(data$cyl)
  model = stats::lm(mpg ~ wt + cylGroup, data = data)
  payload = classifyModelFollowupQuestion(
    "What do similar cars look like for wt = 3 and cylGroup = 6?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(
    model,
    payload,
    neighbourCount = 8L
  )

  html = htmltools::renderTags(
    buildComparableObservationResultUi(payload)
  )$html

  testthat::expect_match(html, "Comparable observations from the fitted data", fixed = TRUE)
  testthat::expect_match(html, "wt = 3", fixed = TRUE)
  testthat::expect_match(html, "Observed mpg", fixed = TRUE)
  testthat::expect_match(html, "Similarity distance", fixed = TRUE)
  testthat::expect_match(html, payload$comparableObservationResult$observations$observation[[1]], fixed = TRUE)
  testthat::expect_match(html, "The table shows the first 5 neighbours", fixed = TRUE)
  testthat::expect_match(html, "do not by themselves establish", fixed = TRUE)
})

testthat::test_that("comparable result UI shows the model scope boundary", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  payload = classifyModelFollowupQuestion(
    "How does this car compare with similar cars for wt = 3?"
  )
  payload = enrichFollowupPayloadWithComparableObservations(model, payload)

  html = htmltools::renderTags(
    buildComparableObservationResultUi(payload)
  )$html

  testthat::expect_match(html, "Comparable observations unavailable", fixed = TRUE)
  testthat::expect_match(html, "ordinary linear models only", fixed = TRUE)
  testthat::expect_false(grepl("<table", html, fixed = TRUE))
})

testthat::test_that("comparable result UI is absent for other follow-up categories", {
  payload = classifyModelFollowupQuestion(
    "What is the expected response when wt is 3?"
  )

  testthat::expect_null(buildComparableObservationResultUi(payload))
})
