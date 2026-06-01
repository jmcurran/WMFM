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
  testthat::expect_null(diamondsII$followupQuestion)
  testthat::expect_false(grepl("10-unit", diamondsIII$followupQuestion, fixed = TRUE))
  testthat::expect_false(grepl("10 unit", diamondsIII$followupQuestion, fixed = TRUE))

  testthat::expect_false(is.ordered(diamondsII$data$cut))
  testthat::expect_false(is.ordered(diamondsII$data$color))
  testthat::expect_false(is.ordered(diamondsII$data$clarity))
  testthat::expect_true(is.factor(diamondsIV$data$cut))
  testthat::expect_true(is.factor(diamondsIV$data$color))
  testthat::expect_true(is.factor(diamondsIV$data$clarity))
  testthat::expect_identical(
    diamondsIV$spec$adjustmentVariables,
    c("cut", "color", "clarity")
  )
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
  testthat::expect_match(result$interpretation, "Starting from a typical carat value", fixed = TRUE)
  testthat::expect_match(result$interpretation, "increasing to", fixed = TRUE)
  testthat::expect_match(result$interpretation, "0.1-unit increase", fixed = TRUE)
  testthat::expect_match(result$interpretation, "fitted price", fixed = TRUE)
  testthat::expect_false(grepl("log(price)", result$interpretation, fixed = TRUE))
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


testthat::test_that("log-log proportional-change follow-ups use percentage predictor changes", {
  data = ggplot2::diamonds[seq_len(300), ]
  model = stats::lm(log(price) ~ log(carat), data = data)

  payload = classifyModelFollowupQuestion(
    "Can you express the weight effect for a 10% increase in carat?"
  )
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)
  result = payload$unitChangeResult

  testthat::expect_identical(payload$category, "proportional_change_request")
  testthat::expect_identical(result$status, "ok")
  testthat::expect_identical(result$modelStructure, "log_log")
  testthat::expect_identical(result$effectScale, "response_multiplier")
  testthat::expect_identical(result$predictorName, "carat")
  testthat::expect_equal(result$requestedPercentChange, 10)
  testthat::expect_equal(result$proportionalPredictorChange, 1.1)
  testthat::expect_true(is.finite(result$percentChange))
  testthat::expect_match(result$interpretation, "10% increase in carat", fixed = TRUE)
})

testthat::test_that("log-log proportional-change follow-ups recognise doubling", {
  data = ggplot2::diamonds[seq_len(300), ]
  model = stats::lm(log(price) ~ log(carat), data = data)

  payload = classifyModelFollowupQuestion(
    "What happens to price if carat doubles?"
  )
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)
  result = payload$unitChangeResult

  testthat::expect_identical(payload$category, "proportional_change_request")
  testthat::expect_identical(result$status, "ok")
  testthat::expect_equal(result$requestedPercentChange, 100)
  testthat::expect_equal(result$proportionalPredictorChange, 2)
  testthat::expect_match(result$interpretation, "100% increase in carat", fixed = TRUE)
})

testthat::test_that("log-log proportional-change prompt block exposes percentage request", {
  data = ggplot2::diamonds[seq_len(300), ]
  model = stats::lm(log(price) ~ log(carat), data = data)
  payload = classifyModelFollowupQuestion(
    "Can you explain the effect as a 10% increase in carat?"
  )
  payload = enrichFollowupPayloadWithUnitChange(model = model, followupPayload = payload)

  block = buildModelFollowupPromptBlock(followupPayload = payload)

  testthat::expect_match(block, "proportional-change interpretation", fixed = TRUE)
  testthat::expect_match(block, "Requested predictor percent change: 10", fixed = TRUE)
  testthat::expect_match(block, "Log-log proportional predictor change: 1.1", fixed = TRUE)
  testthat::expect_false(grepl("elasticity", block, fixed = TRUE))
  testthat::expect_false(grepl("power law", block, fixed = TRUE))
})

testthat::test_that("log-log adjustment follow-up compares adjusted and weight-only models", {
  data = ggplot2::diamonds[seq_len(600), ]
  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)

  payload = classifyModelFollowupQuestion(
    "Does adjusting for cut, color, and clarity improve our predictions substantially?"
  )
  payload = enrichFollowupPayloadWithAdjustmentComparison(model = model, followupPayload = payload)
  result = payload$adjustmentComparisonResult

  testthat::expect_identical(payload$category, "adjustment_prediction_comparison")
  testthat::expect_identical(result$status, "ok")
  testthat::expect_identical(result$modelStructure, "log_log")
  testthat::expect_identical(result$comparisonType, "adjusted_vs_weight_only_log_log")
  testthat::expect_true(all(c("cut", "color", "clarity") %in% result$adjustmentTerms))
  testthat::expect_true(is.finite(result$logLikChange))
  testthat::expect_true(is.finite(result$likelihoodRatioStatistic))
  testthat::expect_true(is.finite(result$likelihoodRatioPValue))
  testthat::expect_true(is.finite(result$aicChange))
  testthat::expect_true(is.finite(result$deviancePercentChange))
  testthat::expect_true(is.list(result$predictionImprovement))
  testthat::expect_true(result$predictionImprovement$category %in% c(
    "substantial_in_sample_improvement",
    "modest_in_sample_improvement",
    "little_in_sample_improvement"
  ))
  testthat::expect_match(result$interpretation, "simpler log-log model", fixed = TRUE)
  testthat::expect_match(result$interpretation, "nested-model fit", fixed = TRUE)
  testthat::expect_match(result$directAnswer, "Direct answer:", fixed = TRUE)
  testthat::expect_match(result$directAnswer, "nested-model log-likelihood", fixed = TRUE)
  testthat::expect_match(result$studentFacingConclusion, "in-sample predictions", fixed = TRUE)
  testthat::expect_match(result$studentFacingConclusion, "Accounting for cut, color, and clarity", fixed = TRUE)
  testthat::expect_false(grepl("^Yes\\.", result$studentFacingConclusion))
  testthat::expect_match(result$directAnswer, "separate test set", fixed = TRUE)
})

testthat::test_that("log-log adjustment follow-up prompt block avoids out-of-sample claims", {
  data = ggplot2::diamonds[seq_len(600), ]
  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)

  payload = classifyModelFollowupQuestion(
    "Does adjusting for cut, color, and clarity improve our predictions substantially?"
  )
  payload = enrichFollowupPayloadWithAdjustmentComparison(model = model, followupPayload = payload)
  block = buildModelFollowupPromptBlock(followupPayload = payload)

  testthat::expect_match(block, "WMFM deterministic adjustment-comparison payload", fixed = TRUE)
  testthat::expect_match(block, "WMFM has already made the model-comparison judgement", fixed = TRUE)
  testthat::expect_match(block, "Student-facing deterministic conclusion", fixed = TRUE)
  testthat::expect_match(block, "do not only describe the adjusted model", ignore.case = TRUE)
  testthat::expect_match(block, "Simpler-model log-likelihood", fixed = TRUE)
  testthat::expect_match(block, "Likelihood-ratio statistic", fixed = TRUE)
  testthat::expect_match(block, "Simpler-model deviance", fixed = TRUE)
  testthat::expect_match(block, "not as proof of better out-of-sample prediction", fixed = TRUE)
  testthat::expect_match(block, "AIC change", fixed = TRUE)
  testthat::expect_match(block, "residual standard error if available", fixed = TRUE)
  testthat::expect_match(block, "Deterministic prediction improvement assessment", fixed = TRUE)
  testthat::expect_match(block, "Deterministic assessment rule", fixed = TRUE)
  testthat::expect_false(grepl("elasticity", block, fixed = TRUE))
  testthat::expect_false(grepl("power law", block, fixed = TRUE))
})


testthat::test_that("adjustment-comparison follow-up appends deterministic student answer", {
  data = ggplot2::diamonds[seq_len(600), ]
  model = stats::lm(log(price) ~ log(carat) + cut + color + clarity, data = data)

  payload = classifyModelFollowupQuestion(
    "Does adjusting for cut, color, and clarity improve our predictions substantially?"
  )
  payload = enrichFollowupPayloadWithAdjustmentComparison(model = model, followupPayload = payload)
  attr(model, "wmfm_model_followup_payload") = payload

  explanation = paste(
    "Diamond prices increase with weight.",
    "After adjusting for cut, color, and clarity, heavier diamonds have higher prices."
  )
  out = appendDeterministicFollowupAnswer(explanation = explanation, model = model)

  testthat::expect_match(out, "Diamond prices increase with weight", fixed = TRUE)
  testthat::expect_match(out, "Accounting for cut, color, and clarity", fixed = TRUE)
  testthat::expect_match(out, "substantially improves the in-sample predictions", fixed = TRUE)
  testthat::expect_match(out, "compared with using weight alone", fixed = TRUE)
  testthat::expect_match(out, "not performance on a separate test set", fixed = TRUE)
  testthat::expect_false(grepl("^Yes\\.", out))
  testthat::expect_false(grepl("log-likelihood", out, fixed = TRUE))
  testthat::expect_false(grepl("AIC", out, fixed = TRUE))
  testthat::expect_false(grepl("deviance", out, fixed = TRUE))
})

testthat::test_that("log-log student-facing guidance uses proportional-change wording", {
  data = ggplot2::diamonds[seq_len(200), ]
  model = stats::lm(log(price) ~ log(carat), data = data)

  profile = buildExplanationModelProfile(
    model = model,
    data = stats::model.frame(model),
    modelType = "lm"
  )
  guidance = buildExplanationScaleGuidance(profile)

  testthat::expect_match(guidance, "percentage-change language", fixed = TRUE)
  testthat::expect_match(guidance, "1% increase in the predictor", fixed = TRUE)
  testthat::expect_match(guidance, "1.88% increase", fixed = TRUE)
  testthat::expect_match(guidance, "Do not interpret the coefficient", fixed = TRUE)
  testthat::expect_false(grepl("taking logs of both", guidance, fixed = TRUE))
  testthat::expect_false(grepl("elasticity", guidance, fixed = TRUE))
  testthat::expect_false(grepl("power law", guidance, fixed = TRUE))
})

testthat::test_that("Diamonds log-log examples keep bounded teaching questions", {
  diamondsII = loadExampleSpec("Diamonds II", package = "WMFM")
  diamondsIII = loadExampleSpec("Diamonds III", package = "WMFM")
  diamondsIV = loadExampleSpec("Diamonds IV", package = "WMFM")

  exampleText = paste(
    diamondsII$researchQuestion,
    diamondsIII$researchQuestion,
    diamondsIII$followupQuestion,
    diamondsIV$researchQuestion,
    diamondsIV$followupQuestion,
    collapse = "\n"
  )

  testthat::expect_match(exampleText, "predict the price of diamonds", fixed = TRUE)
  testthat::expect_match(exampleText, "0.1 carat", fixed = TRUE)
  testthat::expect_match(exampleText, "adjusting for cut, color, and clarity", fixed = TRUE)
  testthat::expect_false(grepl("elasticity", exampleText, fixed = TRUE))
  testthat::expect_false(grepl("power law", exampleText, fixed = TRUE))
})
