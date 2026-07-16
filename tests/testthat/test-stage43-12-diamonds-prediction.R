testthat::test_that("Stage 43.12 predicts Diamonds models fitted from a local formula", {
  fitDiamondsModel = function() {
    data = data.frame(
      price = c(1200, 1800, 2600, 3500, 4700, 6200, 7900, 9800),
      carat = c(0.4, 0.5, 0.65, 0.8, 1.0, 1.2, 1.4, 1.6),
      cut = factor(c("Good", "Ideal", "Good", "Ideal", "Ideal", "Good", "Ideal", "Good")),
      color = factor(c("G", "G", "H", "G", "G", "H", "G", "H")),
      clarity = factor(c("VS1", "VS1", "SI1", "VS1", "VS1", "SI1", "VS1", "SI1"))
    )
    formula = stats::as.formula(
      "log(price) ~ log(carat) + cut + color + clarity"
    )
    stats::lm(formula, data = data)
  }

  model = fitDiamondsModel()
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

testthat::test_that("Stage 43.12 prediction terms environment contains source variables", {
  data = data.frame(
    price = c(1200, 1800, 2600, 3500, 4700),
    carat = c(0.4, 0.5, 0.65, 0.8, 1.0)
  )
  model = stats::lm(log(price) ~ log(carat), data = data)
  newData = data.frame(carat = 0.75)

  predictionModel = prepareLmModelForPrediction(
    model = model,
    newData = newData
  )
  termsEnvironment = environment(stats::terms(predictionModel))

  testthat::expect_true(exists("carat", envir = termsEnvironment, inherits = FALSE))
  testthat::expect_equal(get("carat", envir = termsEnvironment), 0.75)
  testthat::expect_true(is.finite(as.numeric(stats::predict(
    predictionModel,
    newdata = newData
  ))))
})
