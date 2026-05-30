testthat::test_that("Stage 25.6 logistic Assign follow-up can ask for odds", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "A student scored 15 on Assign. What odds of passing would you predict?"
  )

  ref = exp(as.numeric(stats::predict(
    fit,
    newdata = data.frame(Assign = 15),
    type = "link"
  ))[1])

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$responseScale, "odds")
  testthat::expect_identical(out$responseDescription, "odds")
  testthat::expect_equal(out$resolvedPredictorValues$Assign, 15)
  testthat::expect_equal(out$fittedPrediction, ref)
  testthat::expect_true(is.list(out$confidenceInterval))
  testthat::expect_identical(out$confidenceInterval$intervalScale, "odds")
})

testthat::test_that("Stage 25.6 logistic Attend and Test follow-up can ask for odds", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Attend + Test, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "Suppose a student has Test = 10 and Attend = Yes. What odds of passing would you predict?"
  )

  refData = data.frame(
    Attend = factor("Yes", levels = levels(course.df$Attend)),
    Test = 10
  )
  ref = exp(as.numeric(stats::predict(fit, newdata = refData, type = "link"))[1])

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$responseScale, "odds")
  testthat::expect_identical(out$responseDescription, "odds")
  testthat::expect_identical(out$resolvedPredictorValues$Attend, "Yes")
  testthat::expect_equal(out$resolvedPredictorValues$Test, 10)
  testthat::expect_equal(out$fittedPrediction, ref)
  testthat::expect_true(is.list(out$confidenceInterval))
  testthat::expect_identical(out$confidenceInterval$intervalScale, "odds")
})

testthat::test_that("Stage 25.6 deterministic appended answer names odds scale", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5))
  fit = stats::glm(Y ~ X, data = df, family = stats::binomial())
  followup = "What odds for Y would you predict when X = 3?"
  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = fit, followupPayload = payload)
  attr(fit, "wmfm_model_followup_payload") = payload

  answer = appendDeterministicFollowupAnswer("Main answer.", fit)

  testthat::expect_identical(payload$predictionResult$status, "ok")
  testthat::expect_identical(payload$predictionResult$responseDescription, "odds")
  testthat::expect_match(answer, "WMFM predicts odds for Y", fixed = TRUE)
  testthat::expect_match(answer, "predicted odds", fixed = TRUE)
})
