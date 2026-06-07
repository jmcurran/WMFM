test_that("addDerivedVariableToData records recognised transformations", {
  df = data.frame(price = c(10, 20, 40), carat = c(1, 2, 4))

  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  expect_true(logRes$ok)
  expect_s3_class(logRes$transformation, "wmfmVariableTransformation")
  expect_equal(logRes$transformation$variable, "logPrice")
  expect_equal(logRes$transformation$rhs, "log(price)")
  expect_equal(logRes$transformation$sourceVariables, "price")
  expect_equal(logRes$transformation$transformationType, "log")
  expect_equal(logRes$transformation$inverseType, "exp")

  log10Res = addDerivedVariableToData(df, "log10Price = log10(price)")
  expect_true(log10Res$ok)
  expect_equal(log10Res$transformation$transformationType, "log10")
  expect_equal(log10Res$transformation$inverseType, "power10")

  sqrtRes = addDerivedVariableToData(df, "sqrtPrice = sqrt(price)")
  expect_true(sqrtRes$ok)
  expect_equal(sqrtRes$transformation$transformationType, "sqrt")
  expect_equal(sqrtRes$transformation$inverseType, "square")
})

test_that("addDerivedVariableToData records simple arithmetic transformations", {
  df = data.frame(price = c(10, 20, 40), carat = c(1, 2, 4))

  scaledRes = addDerivedVariableToData(df, "priceThousands = price / 1000")
  expect_true(scaledRes$ok)
  expect_equal(scaledRes$transformation$sourceVariables, "price")
  expect_equal(scaledRes$transformation$transformationType, "divideConstant")
  expect_equal(scaledRes$transformation$inverseType, "multiplyConstant")
  expect_equal(scaledRes$transformation$transformationParameters$constant, 1000)

  shiftedRes = addDerivedVariableToData(df, "pricePlusFive = price + 5")
  expect_true(shiftedRes$ok)
  expect_equal(shiftedRes$transformation$sourceVariables, "price")
  expect_equal(shiftedRes$transformation$transformationType, "addConstant")
  expect_equal(shiftedRes$transformation$inverseType, "subtractConstant")
  expect_equal(shiftedRes$transformation$transformationParameters$constant, 5)
})

test_that("failed derived-variable creation does not return transformation metadata", {
  df = data.frame(price = c(10, 20, 40))

  res = addDerivedVariableToData(df, "badVar = unknownColumn + 1")

  expect_false(res$ok)
  expect_null(res$transformation)
})

test_that("formula transformation selection keeps only variables used by the model", {
  df = data.frame(price = c(10, 20, 40), carat = c(1, 2, 4))
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  sqrtRes = addDerivedVariableToData(logRes$data, "sqrtPrice = sqrt(price)")

  records = list(
    logPrice = logRes$transformation,
    sqrtPrice = sqrtRes$transformation
  )

  selected = getFormulaVariableTransformations(
    formula = logPrice ~ carat,
    variableTransformations = records
  )

  expect_named(selected, "logPrice")
  expect_equal(selected$logPrice$transformationType, "log")
})

test_that("runModel preserves transformation records used by the fitted formula", {
  df = data.frame(price = c(10, 20, 40, 80), carat = c(1, 2, 4, 8))
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  sqrtRes = addDerivedVariableToData(logRes$data, "sqrtPrice = sqrt(price)")

  records = list(
    logPrice = logRes$transformation,
    sqrtPrice = sqrtRes$transformation
  )

  fit = suppressWarnings(runModel(
    data = sqrtRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  expect_s3_class(fit, "wmfmModel")
  expect_named(fit$variableTransformations, "logPrice")
  expect_named(getModelVariableTransformations(fit$model), "logPrice")
  expect_equal(fit$variableTransformations$logPrice$inverseType, "exp")
})

test_that("buildModelExplanationAudit records fitted derived-variable transformations", {
  df = data.frame(price = c(10, 20, 40, 80), carat = c(1, 2, 4, 8))
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  audit = fit$explanationAudit

  expect_s3_class(audit, "wmfmExplanationAudit")
  expect_true(is.data.frame(audit$variableTransformations))
  expect_equal(nrow(audit$variableTransformations), 1)
  expect_equal(audit$variableTransformations$variable, "logPrice")
  expect_equal(audit$variableTransformations$sourceVariables, "price")
  expect_equal(audit$variableTransformations$transformationType, "log")
  expect_equal(audit$variableTransformations$inverseType, "exp")
  expect_true(audit$promptInputs$variableTransformationsIncluded)
  expect_match(
    audit$rawPromptIngredients$variableTransformationPrompt,
    "User-created derived variables used by this fitted model",
    fixed = TRUE
  )
})

testthat::test_that("teaching summary describes fitted derived-variable transformations", {
  df = data.frame(
    price = c(10, 20, 40, 80),
    logPrice = log(c(10, 20, 40, 80)),
    carat = c(1, 2, 3, 4)
  )

  records = list(
    logPrice = createVariableTransformationRecord(
      variable = "logPrice",
      expression = "logPrice = log(price)",
      rhs = "log(price)",
      sourceVariables = "price",
      transformationType = "log",
      inverseType = "exp"
    )
  )

  fit = suppressWarnings(runModel(
    formula = logPrice ~ carat,
    data = df,
    variableTransformations = records,
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  audit = buildModelExplanationAudit(fit$model)
  summary = buildExplanationTeachingSummary(audit = audit, model = fit$model)

  testthat::expect_match(
    summary$variableTransformationSummary,
    "user-created derived variable",
    fixed = TRUE
  )
  testthat::expect_match(summary$variableTransformationSummary, "`logPrice`", fixed = TRUE)
  testthat::expect_match(summary$variableTransformationSummary, "`price`", fixed = TRUE)
  testthat::expect_match(
    summary$variableTransformationSummary,
    "does not yet automatically back-transform fitted values or confidence intervals",
    fixed = TRUE
  )

  testthat::expect_true("Derived variables" %in% summary$evidenceTable$section)
})

testthat::test_that("variable-transformation audit distinguishes response and predictor roles", {
  df = data.frame(
    price = c(10, 20, 40, 80),
    logPrice = log(c(10, 20, 40, 80)),
    carat = c(1, 2, 3, 4),
    logCarat = log(c(1, 2, 3, 4))
  )

  records = list(
    logPrice = createVariableTransformationRecord(
      variable = "logPrice",
      rhs = "log(price)",
      sourceVariables = "price",
      transformationType = "log",
      inverseType = "exp"
    ),
    logCarat = createVariableTransformationRecord(
      variable = "logCarat",
      rhs = "log(carat)",
      sourceVariables = "carat",
      transformationType = "log",
      inverseType = "exp"
    )
  )

  responseFit = suppressWarnings(runModel(
    formula = logPrice ~ carat,
    data = df,
    variableTransformations = records,
    generateExplanation = FALSE,
    printOutput = FALSE
  ))
  responseAudit = buildModelExplanationAudit(responseFit$model)

  testthat::expect_equal(responseAudit$variableTransformations$role, "response")
  testthat::expect_match(
    buildVariableTransformationPromptBlock(responseAudit$variableTransformations),
    "Response-variable transformations are the only records that may later support response-scale back-transformation",
    fixed = TRUE
  )

  predictorFit = suppressWarnings(runModel(
    formula = price ~ logCarat,
    data = df,
    variableTransformations = records,
    generateExplanation = FALSE,
    printOutput = FALSE
  ))
  predictorAudit = buildModelExplanationAudit(predictorFit$model)
  predictorSummary = buildExplanationTeachingSummary(audit = predictorAudit, model = predictorFit$model)

  testthat::expect_equal(predictorAudit$variableTransformations$role, "predictor")
  testthat::expect_match(
    predictorSummary$variableTransformationSummary,
    "not a reason to back-transform the response",
    fixed = TRUE
  )
  testthat::expect_false(grepl(
    "may later support response-scale back-transformation",
    predictorSummary$variableTransformationSummary,
    fixed = TRUE
  ))
})

testthat::test_that("variable-transformation audit helpers use stable empty and prompt contracts", {
  emptyTable = emptyVariableTransformationAuditTable()

  testthat::expect_s3_class(emptyTable, "data.frame")
  testthat::expect_equal(nrow(emptyTable), 0)
  testthat::expect_named(emptyTable, c(
    "variable",
    "role",
    "sourceVariables",
    "expression",
    "rhs",
    "transformationType",
    "inverseType",
    "transformationParameters"
  ))

  prompt = buildVariableTransformationPromptBlock(emptyTable)
  testthat::expect_match(
    prompt,
    "none recorded",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "Do not invent back-transformations",
    fixed = TRUE
  )
})

testthat::test_that("teaching summary gives a stable no-transformation note", {
  df = data.frame(y = c(1, 2, 4, 7), x = c(1, 2, 3, 4))
  model = stats::lm(y ~ x, data = df)
  audit = buildModelExplanationAudit(model)
  summary = buildExplanationTeachingSummary(audit = audit, model = model)

  testthat::expect_match(
    summary$variableTransformationSummary,
    "No user-created derived variables were used in this fitted model",
    fixed = TRUE
  )
  testthat::expect_false("Derived variables" %in% summary$evidenceTable$section)
})


testthat::test_that("variable-transformation model tests can disable explanation generation", {
  df = data.frame(price = c(10, 20, 43, 90), carat = c(1, 2, 3, 4))
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  testthat::local_mocked_bindings(
    getChatProvider = function(...) {
      stop("variable-transformation tests must not contact a chat provider", call. = FALSE)
    }
  )

  fit = runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    generateExplanation = FALSE,
    printOutput = FALSE
  )

  testthat::expect_s3_class(fit, "wmfmModel")
  testthat::expect_null(fit$explanation)
  testthat::expect_false(fit$meta$generateExplanation)
  testthat::expect_named(fit$variableTransformations, "logPrice")
})


testthat::test_that("response-transformation handling mode is stored on fitted models", {
  df = data.frame(price = c(10, 20, 43, 90), carat = c(1, 2, 3, 4))
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    responseTransformationMode = "original",
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  testthat::expect_equal(getModelResponseTransformationMode(fit$model), "original")
  testthat::expect_equal(fit$responseTransformationMode, "original")
  testthat::expect_equal(fit$meta$responseTransformationMode, "original")
})

testthat::test_that("response-transformation handling mode validates allowed values", {
  testthat::expect_equal(normaliseResponseTransformationMode(NULL), "both")
  testthat::expect_equal(normaliseResponseTransformationMode("model"), "model")
  testthat::expect_error(
    normaliseResponseTransformationMode("predictor"),
    "must be one of",
    fixed = TRUE
  )
})

testthat::test_that("response back-transformation payload supplies original response quantities", {
  df = data.frame(
    price = c(10, 20, 45, 90, 180),
    carat = c(1, 2, 3, 4, 5)
  )
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    responseTransformationMode = "both",
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  payload = fit$explanationAudit$responseBackTransformations

  testthat::expect_equal(payload$status, "available")
  testthat::expect_equal(payload$responseVariable, "logPrice")
  testthat::expect_equal(payload$originalVariable, "price")
  testthat::expect_equal(payload$inverseType, "exp")
  testthat::expect_true(is.data.frame(payload$table))
  testthat::expect_true(nrow(payload$table) > 0)
  testthat::expect_true(any(payload$table$meaning %in% "original response fitted value"))
  testthat::expect_true(any(payload$table$meaning %in% "original response multiplier"))
  testthat::expect_true(all(payload$table$estimate > 0))

  testthat::expect_match(
    fit$explanationAudit$rawPromptIngredients$responseBackTransformationPrompt,
    "Response back-transformation payload",
    fixed = TRUE
  )
  testthat::expect_match(
    fit$explanationAudit$rawPromptIngredients$responseBackTransformationPrompt,
    "Original response variable: `price`",
    fixed = TRUE
  )
})

testthat::test_that("response back-transformation payload respects model-scale-only mode", {
  df = data.frame(price = c(10, 20, 45, 90), carat = c(1, 2, 3, 4))
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    responseTransformationMode = "model",
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  payload = fit$explanationAudit$responseBackTransformations

  testthat::expect_equal(payload$status, "not_requested")
  testthat::expect_equal(nrow(payload$table), 0)
  testthat::expect_identical(
    fit$explanationAudit$rawPromptIngredients$responseBackTransformationPrompt,
    ""
  )
})

testthat::test_that("response back-transformation is unavailable for predictor-only transformations", {
  df = data.frame(
    price = c(10, 20, 45, 90),
    carat = c(1, 2, 3, 4)
  )
  logRes = addDerivedVariableToData(df, "logCarat = log(carat)")
  records = list(logCarat = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = price ~ logCarat,
    modelType = "lm",
    variableTransformations = records,
    responseTransformationMode = "both",
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  payload = fit$explanationAudit$responseBackTransformations

  testthat::expect_equal(payload$status, "not_available")
  testthat::expect_equal(payload$note, "The fitted response is not a recorded user-created derived variable.")
  testthat::expect_match(
    fit$explanationAudit$rawPromptIngredients$responseBackTransformationPrompt,
    "Do not invent original-response-scale fitted values",
    fixed = TRUE
  )
})

testthat::test_that("response back-transformation prompt keeps both mode on original response scale", {
  df = data.frame(
    price = c(10, 20, 45, 90, 180),
    carat = c(1, 2, 3, 4, 5)
  )
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    responseTransformationMode = "both",
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  responsePrompt = fit$explanationAudit$rawPromptIngredients$responseBackTransformationPrompt
  formattedPrompt = fit$explanationAudit$rawPromptIngredients$formattedQuantityPrompt
  scalePrompt = fit$explanationAudit$rawPromptIngredients$responseScaleControlPrompt
  fullPrompt = lmToExplanationPrompt(fit$model)

  testthat::expect_match(
    responsePrompt,
    "Use the original `price` scale for all substantive fitted values and effect interpretations.",
    fixed = TRUE
  )
  testthat::expect_match(
    responsePrompt,
    "Do not report numeric fitted values or effect sizes for `logPrice`",
    fixed = TRUE
  )
  testthat::expect_identical(formattedPrompt, "")
  testthat::expect_match(
    scalePrompt,
    "Use the response back-transformation payload for substantive fitted values and effects",
    fixed = TRUE
  )
  testthat::expect_match(
    scalePrompt,
    "avoid `multiplies by`",
    fixed = TRUE
  )
  testthat::expect_match(
    responsePrompt,
    "Do not write that the expected response `multiplies by` a value",
    fixed = TRUE
  )
  testthat::expect_no_match(
    fullPrompt,
    "Approximate proportion of variation explained by the model",
    fixed = TRUE
  )
  testthat::expect_no_match(
    fullPrompt,
    "briefly explain what it says about how well",
    fixed = TRUE
  )
})

testthat::test_that("response back-transformation prompt respects original-scale-only mode", {
  df = data.frame(
    price = c(10, 20, 45, 90, 180),
    carat = c(1, 2, 3, 4, 5)
  )
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    responseTransformationMode = "original",
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  responsePrompt = fit$explanationAudit$rawPromptIngredients$responseBackTransformationPrompt
  formattedPrompt = fit$explanationAudit$rawPromptIngredients$formattedQuantityPrompt
  fullPrompt = lmToExplanationPrompt(fit$model)

  testthat::expect_match(
    responsePrompt,
    "Explain fitted values and effects on the original `price` scale only.",
    fixed = TRUE
  )
  testthat::expect_match(
    responsePrompt,
    "Do not describe expected values or effects for `logPrice`",
    fixed = TRUE
  )
  testthat::expect_identical(formattedPrompt, "")
  testthat::expect_no_match(
    fullPrompt,
    "Approximate proportion of variation explained by the model",
    fixed = TRUE
  )
})


testthat::test_that("response back-transformation prompt keeps R-squared on model scale mode", {
  df = data.frame(
    price = c(10, 20, 45, 90, 180),
    carat = c(1, 2, 3, 4, 5)
  )
  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  records = list(logPrice = logRes$transformation)

  fit = suppressWarnings(runModel(
    data = logRes$data,
    formula = logPrice ~ carat,
    modelType = "lm",
    variableTransformations = records,
    responseTransformationMode = "model",
    generateExplanation = FALSE,
    printOutput = FALSE
  ))

  fullPrompt = lmToExplanationPrompt(fit$model)

  testthat::expect_match(
    fullPrompt,
    "Approximate proportion of variation explained by the model",
    fixed = TRUE
  )
})
