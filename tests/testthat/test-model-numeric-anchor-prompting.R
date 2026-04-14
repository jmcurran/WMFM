test_that("buildModelNumericAnchorInfo reports mean anchor when zero is outside the data range", {
  dat = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5, 6, 7, 8)
  )

  fit = stats::lm(y ~ x, data = dat)
  info = buildModelNumericAnchorInfo(fit)

  expect_identical(info$numericReference, "mean")
  expect_match(info$promptText, "x: observed range = \\[5, 8\\]", perl = TRUE)
  expect_match(info$promptText, "chosen anchor = 6.5", fixed = TRUE)
  expect_match(info$promptText, "0 lies outside the observed range", fixed = TRUE)
  expect_match(info$cacheKey, "mean", fixed = TRUE)
  expect_match(info$cacheKey, "x=5:8=6.5", fixed = TRUE)
})

test_that("lmToExplanationPrompt includes numeric anchor guidance", {
  dat = data.frame(
    y = c(2, 3, 5, 7),
    Magnitude = c(5.25, 5.5, 6, 7.25),
    Locn = factor(c("SC", "SC", "WA", "WA"))
  )

  fit = stats::glm(y ~ Locn * Magnitude, family = "poisson", data = dat)
  prompt = lmToExplanationPrompt(fit)

  expect_match(prompt, "Numeric interpretation anchor:", fixed = TRUE)
  expect_match(prompt, "Magnitude: observed range = [5.25, 7.25]", fixed = TRUE)
  expect_match(prompt, "do not describe the intercept as if it were directly meaningful at 0", fixed = TRUE)
})

test_that("lmToPrompt includes numeric anchor guidance for equations", {
  dat = data.frame(
    y = c(2, 3, 5, 7),
    Magnitude = c(5.25, 5.5, 6, 7.25),
    Locn = factor(c("SC", "SC", "WA", "WA"))
  )

  fit = stats::glm(y ~ Locn * Magnitude, family = "poisson", data = dat)
  prompt = lmToPrompt(fit)

  expect_match(prompt, "Numeric interpretation anchor:", fixed = TRUE)
  expect_match(prompt, "Formal fitted equations may still be written as functions of the numeric predictor itself.", fixed = TRUE)
  expect_match(prompt, "Do not describe an intercept or baseline fitted value as being 'when X = 0' unless 0 lies inside the observed range", fixed = TRUE)
})
