testthat::test_that("predicts from natural values for transformed predictors", {
  data = data.frame(
    price = c(1200, 1800, 2600, 3500, 4700, 6200, 7900, 9800),
    carat = c(0.4, 0.5, 0.65, 0.8, 1.0, 1.2, 1.4, 1.6),
    cut = factor(c("Good", "Ideal", "Good", "Ideal", "Ideal", "Good", "Ideal", "Good")),
    color = factor(c("G", "G", "H", "G", "G", "H", "G", "H")),
    clarity = factor(c("VS1", "VS1", "SI1", "VS1", "VS1", "SI1", "VS1", "SI1"))
  )
  model = stats::lm(
    log(price) ~ log(carat) + cut + color + clarity,
    data = data
  )
  question = paste(
    "What price would you predict for a 1.0 carat diamond",
    "with cut Ideal, color G, and clarity VS1?"
  )

  out = computeLmModelQuestionPrediction(model, question)

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$resolvedPredictorValues$carat, 1)
  testthat::expect_identical(out$resolvedPredictorValues$cut, "Ideal")
  testthat::expect_identical(out$resolvedPredictorValues$color, "G")
  testthat::expect_identical(out$resolvedPredictorValues$clarity, "VS1")
  testthat::expect_true(is.finite(out$fittedPrediction))
})

testthat::test_that("newdata uses source variables for simple transformations", {
  data = data.frame(
    response = c(2, 3, 5, 8, 13),
    x = c(1, 2, 3, 4, 5)
  )
  model = stats::lm(log(response) ~ log(x), data = data)

  out = buildLmPredictionNewData(
    model = model,
    suppliedPredictorValues = list("log(x)" = log(3))
  )

  testthat::expect_true(out$ok)
  testthat::expect_true("x" %in% names(out$newData))
  testthat::expect_false("log(x)" %in% names(out$newData))
  testthat::expect_equal(out$newData$x, 3)
})

testthat::test_that("prediction failures return a payload rather than an error", {
  data = data.frame(
    response = c(2, 3, 5, 8, 13),
    x = c(1, 2, 3, 4, 5)
  )
  model = stats::lm(response ~ I(x^2), data = data)

  out = computeLmModelQuestionPrediction(
    model = model,
    followupQuestion = "What response would you predict when I(x^2) = 9?"
  )

  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "prediction_failed")
  testthat::expect_match(
    paste(out$warnings, collapse = " "),
    "could not calculate this prediction",
    fixed = TRUE
  )
})

testthat::test_that("deterministic intervals name the response plainly", {
  data = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("No", "No", "Yes", "Yes", "Yes", "Yes"), levels = c("No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)
  payload = classifyModelFollowupQuestion(
    "What mark would you predict if Attend = Yes and Test = 15?"
  )
  payload = enrichFollowupPayloadWithLmPrediction(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  out = buildDeterministicFollowupAnswer(model)

  testthat::expect_match(out, "prediction interval for Exam", fixed = TRUE)
  testthat::expect_match(out, "estimated average Exam", fixed = TRUE)
  testthat::expect_no_match(out, "people or cases", fixed = TRUE)
  testthat::expect_no_match(out, "individual outcome", fixed = TRUE)
})
