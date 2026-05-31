testthat::test_that("log-log metadata detects simple log-log models", {
  data = ggplot2::diamonds[seq_len(200), ]
  model = stats::lm(log(price) ~ log(carat), data = data)

  out = getLogLogModelMetadata(model = model, modelFrame = stats::model.frame(model))

  testthat::expect_true(out$isLogLog)
  testthat::expect_identical(out$responseVariable, "price")
  testthat::expect_equal(nrow(out$logPredictors), 1)
  testthat::expect_identical(out$logPredictors$originalName[[1]], "carat")
})

testthat::test_that("log-log profile records proportional-change interpretation scale", {
  data = ggplot2::diamonds[seq_len(200), ]
  model = stats::lm(log(price) ~ log(carat), data = data)

  out = buildExplanationModelProfile(model = model, data = stats::model.frame(model), modelType = "lm")

  testthat::expect_identical(out$responseVariable, "price")
  testthat::expect_identical(out$transformationType, "log")
  testthat::expect_true(out$logLog$isLogLog)
  testthat::expect_identical(out$predictorTypes$numeric, "log(carat)")
  testthat::expect_identical(out$interpretationScale, "logLogProportionalChange")
})

testthat::test_that("log-log equation display includes original-scale multiplicative expression", {
  data = ggplot2::diamonds[seq_len(200), ]
  model = stats::lm(log(price) ~ log(carat), data = data)

  out = buildDeterministicEquationTable(model = model, digits = 2)

  testthat::expect_true(any(grepl("log\\(price\\)", out$linearPredictor)))
  testthat::expect_true(any(grepl("price = ", out$responseScale, fixed = TRUE)))
  testthat::expect_true(any(grepl("carat\\^", out$responseScale)))
})

testthat::test_that("adjusted log-log equation display keeps factor-specific original-scale equations", {
  data = ggplot2::diamonds[seq_len(600), ]
  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)

  out = buildDeterministicEquationTable(model = model, digits = 2)

  testthat::expect_true(any(grepl("price = ", out$responseScale, fixed = TRUE)))
  testthat::expect_true(any(grepl("carat\\^", out$responseScale)))
  testthat::expect_true(any(grepl("cut", out$condition, fixed = TRUE)))
  testthat::expect_true(any(grepl("color", out$condition, fixed = TRUE)))
  testthat::expect_true(any(grepl("clarity", out$condition, fixed = TRUE)))
})

testthat::test_that("Diamonds II to IV examples load with log-log formulas", {
  examples = listWMFMExamples(package = "WMFM")

  testthat::expect_true("Diamonds II" %in% examples)
  testthat::expect_true("Diamonds III" %in% examples)
  testthat::expect_true("Diamonds IV" %in% examples)

  diamondsII = loadExampleSpec("Diamonds II", package = "WMFM")
  diamondsIII = loadExampleSpec("Diamonds III", package = "WMFM")
  diamondsIV = loadExampleSpec("Diamonds IV", package = "WMFM")

  testthat::expect_identical(diamondsII$spec$formula, "log(price) ~ log(carat)")
  testthat::expect_identical(diamondsIII$spec$formula, "log(price) ~ log(carat)")
  testthat::expect_identical(
    diamondsIV$spec$formula,
    "log(price) ~ log(carat) + cut + color + clarity"
  )
  testthat::expect_match(diamondsIII$followupQuestion, "0.1 carat", fixed = TRUE)
  testthat::expect_match(diamondsIV$followupQuestion, "adjusting for cut, color, and clarity", fixed = TRUE)
})

testthat::test_that("log-log unit-change follow-ups use original-scale proportional changes", {
  data = ggplot2::diamonds[seq_len(300), ]
  model = stats::lm(log(price) ~ log(carat), data = data)

  result = computeModelQuestionUnitChange(
    model = model,
    followupQuestion = "Can you express the weight effect for a 0.1 carat increase?",
    requestedUnitChange = 0.1
  )

  testthat::expect_identical(result$status, "ok")
  testthat::expect_identical(result$modelStructure, "log_log")
  testthat::expect_identical(result$effectScale, "response_multiplier")
  testthat::expect_identical(result$predictorName, "carat")
  testthat::expect_identical(result$transformedPredictorName, "log(carat)")
  testthat::expect_true(is.finite(result$referenceValue))
  testthat::expect_gt(result$referenceValue, 0)
  testthat::expect_equal(result$comparisonValue, result$referenceValue + 0.1)
  testthat::expect_true(is.finite(result$percentChange))
  testthat::expect_match(result$interpretation, "typical carat value", fixed = TRUE)
})

testthat::test_that("log-log unit-change prompt block exposes deterministic reference values", {
  data = ggplot2::diamonds[seq_len(300), ]
  model = stats::lm(log(price) ~ log(carat), data = data)
  payload = classifyModelFollowupQuestion(
    "Can you express the weight effect for a 0.1 carat increase?"
  )
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)

  block = buildModelFollowupPromptBlock(followupPayload = payload)

  testthat::expect_match(block, "Log-log original-scale reference value", fixed = TRUE)
  testthat::expect_match(block, "Log-log proportional predictor change", fixed = TRUE)
  testthat::expect_match(block, "Requested unit-change percent interpretation", fixed = TRUE)
  testthat::expect_false(grepl("elasticity", block, fixed = TRUE))
  testthat::expect_false(grepl("power law", block, fixed = TRUE))
})
