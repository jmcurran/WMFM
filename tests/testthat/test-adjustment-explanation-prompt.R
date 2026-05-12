testthat::test_that("prompt separates primary predictors and adjustment variables", {
  dat = data.frame(y = c(1, 2, 3, 4, 5), x = c(2, 3, 4, 5, 6), age = c(20, 21, 19, 22, 20))
  fit = stats::lm(y ~ x + age, data = dat)
  attr(fit, "wmfm_adjustment_variables") = "age"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Adjustment-variable interpretation policy (version: stage20.13-v1):", fixed = TRUE)
  testthat::expect_match(prompt, "Primary predictors: x", fixed = TRUE)
  testthat::expect_match(prompt, "Adjustment variables: age", fixed = TRUE)
  testthat::expect_match(prompt, "Do not interpret adjustment-variable coefficients as substantive findings.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not interpret interaction terms that include any adjustment variable as main findings.", fixed = TRUE)
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

  testthat::expect_match(block, "age: .*adjustment variable", perl = TRUE)
  testthat::expect_match(block, "mention age only as adjusted-for variables", fixed = TRUE)
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
  testthat::expect_no_match(prompt, "Adjustment-variable interpretation policy (version: stage20.13-v1):", fixed = TRUE)

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

  testthat::expect_match(prompt, "Adjustment variables: picture", fixed = TRUE)
  testthat::expect_match(prompt, "Primary predictors: gender", fixed = TRUE)
  testthat::expect_match(prompt, "Omitted adjustment-related terms in explanation payload: picture, gender:picture", fixed = TRUE)
  testthat::expect_match(prompt, "Answer the research question using the variables of scientific interest", fixed = TRUE)
  testthat::expect_match(prompt, "after adjusting for picture", fixed = TRUE)
  testthat::expect_match(prompt, "Do not interpret adjustment-variable coefficients as substantive findings.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not interpret interaction terms that include any adjustment variable as main findings.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not provide adjustment-level-specific or picture-specific effect estimates", fixed = TRUE)
  testthat::expect_match(prompt, "high-level model-structure note", fixed = TRUE)
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
