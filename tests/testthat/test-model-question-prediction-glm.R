testthat::test_that("logistic mean prediction matches stats predict response", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3")
  ref = as.numeric(stats::predict(model, newdata = data.frame(X = 3), type = "response"))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$modelType, "glm")
  testthat::expect_identical(out$predictionType, "mean_response_prediction")
  testthat::expect_equal(out$fittedPrediction, ref)
})

testthat::test_that("poisson mean prediction matches stats predict response", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3")
  ref = as.numeric(stats::predict(model, newdata = data.frame(X = 3), type = "response"))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$fittedPrediction, ref)
})

testthat::test_that("GLM prediction with missing covariates fails safely", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5), G = factor(c("A", "A", "B", "B", "A", "B", "A", "B")))
  model = stats::glm(Y ~ X + G, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3")
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "missing_predictor_values")
})

testthat::test_that("GLM interval requests are unsupported safely", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3 with confidence interval")
  testthat::expect_identical(out$status, "unsupported")
  testthat::expect_identical(out$reason, "unsupported_glm_interval_request")
})
