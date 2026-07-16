testthat::test_that("linear predictions support transformed predictors from local formulas", {
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

testthat::test_that("prediction terms environments contain source variables", {
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

testthat::test_that("confidence interval predictions use source variables for transformed terms", {
  data = expand.grid(
    carat = c(0.5, 0.8, 1.1, 1.4),
    cut = c("Good", "Ideal"),
    color = c("G", "H"),
    clarity = c("SI1", "VS1"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  data$cut = factor(data$cut)
  data$color = factor(data$color)
  data$clarity = factor(data$clarity)
  data$price = exp(
    7 +
      0.9 * log(data$carat) +
      0.12 * (data$cut == "Ideal") -
      0.08 * (data$color == "H") +
      0.15 * (data$clarity == "VS1")
  )

  modelResult = runModel(
    data = data,
    formula = log(price) ~ log(carat) + cut + color + clarity,
    modelType = "lm",
    dataContext = "price: Diamond price",
    followupQuestion = paste(
      "What price would you predict for a 1.0 carat diamond",
      "with cut Ideal, color G, and clarity VS1?"
    ),
    generateExplanation = FALSE,
    printOutput = FALSE
  )

  payload = attr(modelResult$model, "wmfm_model_followup_payload")$predictionResult

  testthat::expect_identical(payload$status, "ok")
  testthat::expect_equal(payload$resolvedPredictorValues$carat, 1)
  testthat::expect_true(is.finite(payload$fittedPrediction))
})


testthat::test_that("factor comparison weights use source variables for transformed terms", {
  data = expand.grid(
    carat = c(0.5, 0.8, 1.1, 1.4),
    cut = c("Good", "Ideal"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  data$cut = factor(data$cut)
  data$price = exp(
    7 +
      0.9 * log(data$carat) +
      0.12 * (data$cut == "Ideal")
  )
  model = stats::lm(log(price) ~ log(carat) + cut, data = data)
  referenceData = data.frame(
    `log(carat)` = log(0.8),
    cut = factor("Good", levels = levels(data$cut)),
    check.names = FALSE
  )
  comparisonData = data.frame(
    `log(carat)` = log(0.8),
    cut = factor("Ideal", levels = levels(data$cut)),
    check.names = FALSE
  )

  weights = buildNewDataDifferenceWeights(
    model = model,
    referenceData = referenceData,
    comparisonData = comparisonData
  )

  testthat::expect_equal(unname(weights[["cutIdeal"]]), 1)
})

testthat::test_that("transformed response predictions are reported on the original scale", {
  data = expand.grid(
    carat = c(0.5, 0.8, 1.1, 1.4),
    cut = c("Good", "Ideal"),
    color = c("G", "H"),
    clarity = c("SI1", "VS1"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  data$cut = factor(data$cut)
  data$color = factor(data$color)
  data$clarity = factor(data$clarity)
  data$price = exp(
    7 +
      0.9 * log(data$carat) +
      0.12 * (data$cut == "Ideal") -
      0.08 * (data$color == "H") +
      0.15 * (data$clarity == "VS1")
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
  modelScaleFit = as.numeric(stats::predict(
    model,
    newdata = data.frame(
      carat = 1,
      cut = factor("Ideal", levels = levels(data$cut)),
      color = factor("G", levels = levels(data$color)),
      clarity = factor("VS1", levels = levels(data$clarity))
    )
  ))

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$responseScale, "original_response")
  testthat::expect_identical(out$originalResponseVariable, "price")
  testthat::expect_equal(out$fittedPrediction, exp(modelScaleFit))
  testthat::expect_equal(
    out$modelScalePrediction$fittedPrediction,
    modelScaleFit
  )
  testthat::expect_equal(
    out$confidenceInterval$lwr,
    exp(out$modelScalePrediction$confidenceInterval$lwr)
  )
  testthat::expect_equal(
    out$predictionInterval$upr,
    exp(out$modelScalePrediction$predictionInterval$upr)
  )
})

testthat::test_that("deterministic answers name the original transformed response", {
  data = data.frame(
    price = c(1000, 1400, 2000, 2800, 3900, 5400),
    carat = c(0.4, 0.5, 0.65, 0.8, 1.0, 1.2)
  )
  model = stats::lm(log(price) ~ log(carat), data = data)
  question = "What price would you predict for a 1.0 carat diamond?"
  payload = classifyModelFollowupQuestion(question)
  payload = enrichFollowupPayloadWithLmPrediction(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  out = buildDeterministicFollowupAnswer(model)

  testthat::expect_match(out, "expected price", fixed = TRUE)
  testthat::expect_match(out, "prediction interval for price", fixed = TRUE)
  testthat::expect_no_match(out, "log(price)", fixed = TRUE)
})
