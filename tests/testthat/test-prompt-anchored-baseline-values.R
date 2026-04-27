test_that("buildAnchoredBaselinePromptBlock includes anchored Poisson fitted values", {
  dat = data.frame(
    Locn = factor(c(rep("SC", 9), rep("WA", 9))),
    Magnitude = c(
      5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25,
      5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25
    ),
    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0)
  )

  fit = stats::glm(Freq ~ Locn * Magnitude, family = "poisson", data = dat)

  block = suppressWarnings(buildAnchoredBaselinePromptBlock(fit))

  expect_match(block, "Precomputed anchored baseline fitted values:", fixed = TRUE)
  expect_match(block, "Do not derive baseline fitted values from the intercept alone.", fixed = TRUE)
  expect_match(block, "estimate = 6.8", fixed = TRUE)
  expect_match(block, "estimate = 0.54", fixed = TRUE)
  expect_match(block, "settings: Locn = SC; Magnitude = 6.25.", fixed = TRUE)
  expect_match(block, "settings: Locn = WA; Magnitude = 6.25.", fixed = TRUE)
})

test_that("lmToExplanationPrompt includes precomputed anchored baseline values", {
  dat = data.frame(
    Locn = factor(c(rep("SC", 9), rep("WA", 9))),
    Magnitude = c(
      5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25,
      5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25
    ),
    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0)
  )

  fit = stats::glm(Freq ~ Locn * Magnitude, family = "poisson", data = dat)

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  expect_match(prompt, "Precomputed anchored baseline fitted values:", fixed = TRUE)
  expect_match(prompt, "Do not derive baseline fitted values from the intercept alone.", fixed = TRUE)
  expect_match(prompt, "estimate = 6.8", fixed = TRUE)
  expect_match(prompt, "estimate = 0.54", fixed = TRUE)
  expect_equal(length(gregexpr("You are writing an overall explanation of a fitted model in plain language\\.", prompt)[[1]]), 1L)
})

test_that("buildAnchoredBaselinePromptBlock uses probability baselines for logistic models", {
  stats20xPath = system.file("extdata", "STATS20x.txt", package = "WMFM")

  if (!nzchar(stats20xPath)) {
    sourcePath = file.path("inst", "extdata", "STATS20x.txt")
    if (file.exists(sourcePath)) {
      stats20xPath = sourcePath
    }
  }

  expect_true(nzchar(stats20xPath))

  courseDf = utils::read.table(
    stats20xPath,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = TRUE
  )

  fit = stats::glm(Pass ~ Assign, family = stats::binomial(), data = courseDf)
  block = suppressWarnings(buildAnchoredBaselinePromptBlock(fit))

  expect_match(block, "Precomputed anchored baseline fitted values:", fixed = TRUE)
  expect_match(block, "Pr(", fixed = TRUE)
  expect_match(block, "%", fixed = TRUE)
  expect_no_match(block, "Odds(", fixed = TRUE)
  expect_no_match(block, "Assign = 0.", fixed = TRUE)
})
