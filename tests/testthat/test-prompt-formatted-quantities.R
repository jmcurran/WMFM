testthat::test_that("lmToExplanationPrompt uses formatted quantities instead of raw prompt tables", {
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

  testthat::expect_match(prompt, "Formatted model quantities for the explanation:", fixed = TRUE)
  testthat::expect_match(prompt, "Do not round, exponentiate, back-transform, or recompute them.", fixed = TRUE)
  testthat::expect_match(prompt, "Baseline or fitted values:", fixed = TRUE)
  testthat::expect_match(prompt, "Effects and comparisons:", fixed = TRUE)
  testthat::expect_no_match(prompt, "Coefficient table:", fixed = TRUE)
  testthat::expect_no_match(prompt, "Confidence intervals (95%):", fixed = TRUE)
  testthat::expect_no_match(prompt, "LocnWA:Magnitude", fixed = TRUE)
})

testthat::test_that("buildModelExplanationAudit records formatted prompt quantities", {
  dat = data.frame(
    y = c(1, 3, 4, 6, 8, 9),
    x = c(0, 1, 2, 3, 4, 5)
  )

  fit = stats::lm(y ~ x, data = dat)
  audit = buildModelExplanationAudit(fit)

  testthat::expect_true(audit$promptInputs$coefficientTableIncluded)
  testthat::expect_true(audit$promptInputs$confidenceIntervalsIncluded)
  testthat::expect_true(audit$promptInputs$formattedQuantitiesIncluded)
  testthat::expect_true(audit$promptInputs$rawCoefficientTableRetainedInAudit)
  testthat::expect_match(
    audit$rawPromptIngredients$formattedQuantityPrompt,
    "Formatted model quantities for the explanation:",
    fixed = TRUE
  )
  testthat::expect_true(is.data.frame(audit$coefficientTable))
})

testthat::test_that("logistic formatted prompt quantities prefer probability baselines and odds multipliers", {
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
  promptBlock = suppressWarnings(buildFormattedPromptQuantityBlock(fit))

  testthat::expect_match(promptBlock, "Baseline or fitted values:", fixed = TRUE)
  testthat::expect_match(promptBlock, "probability scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "odds multiplier scale", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "odds scale", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "1.9:1", fixed = TRUE)
})


testthat::test_that("logistic factor formatted prompt quantities include a direct odds ratio", {
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

  fit = stats::glm(Pass ~ Attend, family = stats::binomial(), data = courseDf)
  promptBlock = suppressWarnings(buildFormattedPromptQuantityBlock(fit))

  testthat::expect_match(promptBlock, "Baseline or fitted values:", fixed = TRUE)
  testthat::expect_match(promptBlock, "probability scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "odds ratio scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "odds ratio comparing", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "odds scale", fixed = TRUE)
})
