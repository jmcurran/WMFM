testthat::test_that("prompt separates primary predictors and adjustment variables", {
  dat = data.frame(y = c(1, 2, 3, 4, 5), x = c(2, 3, 4, 5, 6), age = c(20, 21, 19, 22, 20))
  fit = stats::lm(y ~ x + age, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "age"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Interpretation policy:", fixed = TRUE)
  testthat::expect_match(prompt, "Primary predictors: x", fixed = TRUE)
  testthat::expect_match(prompt, "The following variables are adjustment variables: age", fixed = TRUE)
  testthat::expect_match(prompt, "Do not interpret adjustment-variable coefficients, contrasts, confidence intervals, fitted means, predicted values, or model terms as findings.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not use adjustment variables as narrative axes.", fixed = TRUE)
  testthat::expect_match(prompt, "after adjusting for age", fixed = TRUE)
  testthat::expect_match(prompt, "do not infer causality from adjustment", ignore.case = TRUE)
})

testthat::test_that("term-evidence guidance marks adjustment variables as adjusted-for only", {
  set.seed(1)
  dat = data.frame(
    y = rnorm(120),
    studyHours = rnorm(120),
    age = rnorm(120)
  )
  fit = stats::lm(y ~ studyHours + age, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "age"

  block = buildLmTermEvidencePromptBlock(fit)

  testthat::expect_no_match(block, "age: .*term-level evidence", perl = TRUE)
  testthat::expect_no_match(block, "For weak additive terms \\(age\\)", perl = TRUE)
})

testthat::test_that("non-adjustment predictors keep substantive interpretation guidance", {
  set.seed(2)
  dat = data.frame(y = rnorm(100), x = rnorm(100), z = rnorm(100))
  fit = stats::lm(y ~ x + z, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "z"

  block = buildAdjustmentVariablePromptBlock(fit)

  testthat::expect_match(block, "Primary predictors: x", fixed = TRUE)
  testthat::expect_match(block, "Interpret primary predictors as the substantive findings of interest.", fixed = TRUE)
})

testthat::test_that("prompts are unchanged for no-adjustment models", {
  dat = data.frame(y = c(1, 2, 3, 4), x = c(2, 3, 4, 5))
  fit = stats::lm(y ~ x, data = dat)
  attr(fit, "wmfm_adjustment_variables") = character(0)

  prompt = suppressWarnings(lmToExplanationPrompt(fit))
  testthat::expect_no_match(prompt, "Interpretation policy:", fixed = TRUE)

  block = buildAdjustmentVariablePromptBlock(fit)
  testthat::expect_identical(block, "")
})


testthat::test_that("adjustment prompt includes interaction guardrails for picture adjustments", {
  dat = data.frame(
    arousal = c(0.2, 0.3, 0.4, 0.1, 0.5, 0.45),
    gender = factor(c("f", "m", "f", "m", "f", "m")),
    picture = factor(c("A", "A", "B", "B", "C", "C"))
  )
  fit = stats::lm(arousal ~ gender + picture + gender:picture, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "picture"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "The following variables are adjustment variables: picture", fixed = TRUE)
  testthat::expect_match(prompt, "Primary predictors: gender", fixed = TRUE)
  testthat::expect_match(prompt, "Omitted adjustment-related terms in explanation payload: picture, gender:picture", fixed = TRUE)
  testthat::expect_match(prompt, "Answer the research question using the variables of scientific interest", fixed = TRUE)
  testthat::expect_match(prompt, "after adjusting for picture", fixed = TRUE)
  testthat::expect_match(prompt, "Do not interpret adjustment-variable coefficients, contrasts, confidence intervals, fitted means, predicted values, or model terms as findings.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not discuss results separately by levels or values of adjustment variables.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not use adjustment variables as narrative axes.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not interpret interactions involving adjustment variables level by level.", fixed = TRUE)
  testthat::expect_match(prompt, "Model-structure note:", fixed = TRUE)
})

testthat::test_that("formatted explanation quantity payload omits adjustment-related rows", {
  quantityTable = data.frame(
    ciSection = c("effect", "effect", "effect"),
    quantity = c("gendermale", "pictureNude", "gendermale:pictureNude"),
    scale = c("response", "response", "response"),
    estimate = c(1.1, 2.2, 3.3),
    lower = c(0.5, 1.1, 2.0),
    upper = c(1.7, 3.3, 4.6),
    stringsAsFactors = FALSE
  )

  dat = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    gender = factor(c("f", "m", "f", "m", "f", "m")),
    picture = factor(c("A", "A", "B", "B", "C", "C"))
  )
  fit = stats::lm(y ~ gender + picture + gender:picture, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "picture"

  filtered = filterExplanationCoefficientPayloadByAdjustmentPolicy(
    quantityTable = quantityTable,
    model = fit
  )

  testthat::expect_identical(filtered$quantity, "gendermale")
})

testthat::test_that("narrative payload filter removes adjustment-level labels from interpretable rows", {
  dat = data.frame(
    arousal = c(0.2, 0.3, 0.4, 0.1, 0.5, 0.45, 0.35, 0.38),
    gender = factor(c("f", "m", "f", "m", "f", "m", "f", "m")),
    picture = factor(c("infant", "infant", "landscape", "landscape", "nude.f", "nude.f", "nude.m", "nude.m"))
  )
  fit = stats::lm(arousal ~ gender + picture + gender:picture, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "picture"

  payload = data.frame(
    quantity = c(
      "genderm",
      "Mean response for infant",
      "contrast male minus female at picture=nude.f",
      "genderm:picturelandscape"
    ),
    ciSection = c("effect", "baseline", "contrast", "effect"),
    stringsAsFactors = FALSE
  )

  filtered = filterExplanationPayloadForAdjustmentVariables(
    payloadTable = payload,
    model = fit,
    labelColumns = c("quantity")
  )

  testthat::expect_identical(filtered$quantity, "genderm")
})

testthat::test_that("narrative payload filter removes adjustment-grouped summaries across payload columns", {
  dat = data.frame(y = rnorm(10), primaryVar = rnorm(10), controlVar = rep(c("A", "B"), 5))
  fit = stats::lm(y ~ primaryVar + controlVar + primaryVar:controlVar, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "controlVar"

  payload = data.frame(
    term = c("primaryVar", "controlVarB", "primaryVar:controlVarB"),
    quantity = c("slope for primaryVar", "Predicted mean at controlVar=B", "difference by controlVar"),
    comparison = c("primaryVar increase", "A vs B for controlVar", "primaryVar by controlVar"),
    stringsAsFactors = FALSE
  )

  filtered = filterExplanationPayloadForAdjustmentVariables(
    payloadTable = payload,
    model = fit,
    labelColumns = c("term", "quantity", "comparison")
  )

  testthat::expect_identical(filtered$term, "primaryVar")
})
