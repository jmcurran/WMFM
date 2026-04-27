testthat::test_that("lmToExplanationPrompt includes logistic response-scale control", {
  stats20xPath = system.file("extdata", "STATS20x.txt", package = "WMFM")

  if (!nzchar(stats20xPath)) {
    sourcePath = file.path("inst", "extdata", "STATS20x.txt")
    if (file.exists(sourcePath)) {
      stats20xPath = sourcePath
    }
  }

  testthat::expect_true(nzchar(stats20xPath))

  courseDf = utils::read.table(
    stats20xPath,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = TRUE
  )

  fit = stats::glm(Pass ~ Assign, family = stats::binomial(), data = courseDf)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Response-scale control:", fixed = TRUE)
  testthat::expect_match(prompt, "Student-facing interpretation scale: probability", fixed = TRUE)
  testthat::expect_match(prompt, "For fitted values, use probability language rather than log-odds language.", fixed = TRUE)
  testthat::expect_match(prompt, "Describe fitted values as probabilities.", fixed = TRUE)
  testthat::expect_match(prompt, "Describe numeric effects as odds multipliers and direct factor comparisons as odds ratios when those quantities are supplied.", fixed = TRUE)
  testthat::expect_no_match(prompt, "Do not talk about probabilities.", fixed = TRUE)
  testthat::expect_no_match(prompt, "Do not use the term \"odds ratio\".", fixed = TRUE)
  testthat::expect_match(prompt, "Use odds ratios or odds multipliers only for effect sizes and direct comparisons.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not mix raw odds, probabilities, and odds ratios in the same reasoning chain", fixed = TRUE)
  testthat::expect_match(prompt, "For factor comparisons, use the supplied odds ratio", fixed = TRUE)
  testthat::expect_match(prompt, "Do not use overlap or non-overlap of separate fitted-value intervals", fixed = TRUE)
  testthat::expect_match(prompt, "Do not describe raw coefficients as the substantive effects.", fixed = TRUE)
})

testthat::test_that("lmToExplanationPrompt includes Poisson response-scale control", {
  dat = data.frame(
    count = c(1, 2, 2, 4, 5, 8, 9, 12),
    dose = c(0, 1, 2, 3, 4, 5, 6, 7)
  )

  fit = stats::glm(count ~ dose, family = stats::poisson(), data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Response-scale control:", fixed = TRUE)
  testthat::expect_match(prompt, "For fitted values, use expected-count language rather than log-count language.", fixed = TRUE)
  testthat::expect_match(prompt, "multiplicative expected-count language", fixed = TRUE)
  testthat::expect_match(prompt, "Do not write log expected count", fixed = TRUE)
})

testthat::test_that("lmToExplanationPrompt treats inline and derived log responses consistently", {
  dat = data.frame(
    y = c(2, 3, 5, 8, 13, 21),
    log.y = log(c(2, 3, 5, 8, 13, 21)),
    x = c(0, 1, 2, 3, 4, 5)
  )

  inlineFit = stats::lm(log(y) ~ x, data = dat)
  derivedFit = stats::lm(log.y ~ x, data = dat)

  inlinePrompt = suppressWarnings(lmToExplanationPrompt(inlineFit))
  derivedPrompt = suppressWarnings(lmToExplanationPrompt(derivedFit))

  expectedText = "The response was modelled after a transformation; treat inline transformations and derived transformed response variables consistently."

  testthat::expect_match(inlinePrompt, expectedText, fixed = TRUE)
  testthat::expect_match(derivedPrompt, expectedText, fixed = TRUE)
  testthat::expect_match(inlinePrompt, "Response transformation type: log", fixed = TRUE)
  testthat::expect_match(derivedPrompt, "Response transformation type: log", fixed = TRUE)
})

testthat::test_that("buildModelExplanationAudit records response-scale control prompt input", {
  dat = data.frame(
    y = c(2, 3, 5, 8, 13, 21),
    x = c(0, 1, 2, 3, 4, 5)
  )

  fit = stats::lm(log(y) ~ x, data = dat)
  audit = buildModelExplanationAudit(fit)

  testthat::expect_true(audit$promptInputs$responseScaleControlIncluded)
  testthat::expect_match(
    audit$rawPromptIngredients$responseScaleControlPrompt,
    "Response-scale control:",
    fixed = TRUE
  )
})
