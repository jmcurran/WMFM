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

testthat::test_that("intercept-only lm formatted prompt quantities include the mean and interval", {
  dat = data.frame(
    Exam = c(60, 70, 80, 90)
  )

  fit = stats::lm(Exam ~ 1, data = dat)
  promptBlock = suppressWarnings(buildFormattedPromptQuantityBlock(fit))

  testthat::expect_match(promptBlock, "Formatted model quantities for the explanation:", fixed = TRUE)
  testthat::expect_match(promptBlock, "Baseline or fitted values:", fixed = TRUE)
  testthat::expect_match(promptBlock, "Mean Exam", fixed = TRUE)
  testthat::expect_match(promptBlock, "estimate =", fixed = TRUE)
  testthat::expect_match(promptBlock, "95% confidence interval", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "(Intercept)", fixed = TRUE)
})

testthat::test_that("two-factor lm interaction prompt includes formatted quantities", {

  d = data.frame(
    Exam = c(44, 46, 59, 61, 40, 42, 56, 58),
    Attend = factor(rep(c("No", "Yes"), times = 4), levels = c("No", "Yes")),
    Gender = factor(rep(c("Female", "Female", "Male", "Male"), each = 2), levels = c("Female", "Male"))
  )

  fit = stats::lm(Exam ~ Attend * Gender, data = d)
  promptBlock = suppressWarnings(buildFormattedPromptQuantityBlock(fit))

  testthat::expect_match(promptBlock, "Formatted model quantities for the explanation:", fixed = TRUE)
  testthat::expect_match(promptBlock, "Expected Exam when Attend = No; Gender = Female", fixed = TRUE)
  testthat::expect_match(promptBlock, "Difference in Exam comparing Attend = Yes with Attend = No when Gender = Female", fixed = TRUE)
  testthat::expect_match(promptBlock, "Difference between Attend differences in Exam for Gender = Male versus Gender = Female", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "Coefficient table:", fixed = TRUE)
})

testthat::test_that("intercept-only logistic formatted prompt quantities include the success probability", {
  dat = data.frame(
    Pass = factor(
      c("Fail", "Fail", "Fail", "Pass", "Pass", "Pass", "Pass", "Pass"),
      levels = c("Fail", "Pass")
    )
  )

  fit = stats::glm(Pass ~ 1, family = stats::binomial(), data = dat)
  promptBlock = suppressWarnings(buildFormattedPromptQuantityBlock(fit))

  testthat::expect_match(promptBlock, "Formatted model quantities for the explanation:", fixed = TRUE)
  testthat::expect_match(promptBlock, "Baseline or fitted values:", fixed = TRUE)
  testthat::expect_match(promptBlock, "Pr(Pass = Pass) on the probability scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "estimate =", fixed = TRUE)
  testthat::expect_match(promptBlock, "95% confidence interval", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "(Intercept)", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "coefficient scale", fixed = TRUE)
})

testthat::test_that("intercept-only logistic explanation prompt includes probability payload", {
  dat = data.frame(
    Pass = factor(
      c("Fail", "Fail", "Fail", "Pass", "Pass", "Pass", "Pass", "Pass"),
      levels = c("Fail", "Pass")
    )
  )

  fit = stats::glm(Pass ~ 1, family = stats::binomial(), data = dat)
  attr(fit, "wmfm_research_question") = "What is the overall chance of passing?"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Pr(Pass = Pass) on the probability scale", fixed = TRUE)
  testthat::expect_match(prompt, "For intercept-only models", fixed = TRUE)
  testthat::expect_no_match(prompt, "coefficient scale", fixed = TRUE)
})

testthat::test_that("two-factor logistic interaction prompt includes probability and odds-ratio payload", {
  d = data.frame(
    Attend = factor(character(0), levels = c("No", "Yes")),
    Gender = factor(character(0), levels = c("Female", "Male")),
    Pass = factor(character(0), levels = c("Fail", "Pass"))
  )

  addRows = function(attend, gender, failures, passes) {
    data.frame(
      Attend = factor(rep(attend, failures + passes), levels = c("No", "Yes")),
      Gender = factor(rep(gender, failures + passes), levels = c("Female", "Male")),
      Pass = factor(c(rep("Fail", failures), rep("Pass", passes)), levels = c("Fail", "Pass"))
    )
  }

  d = rbind(
    d,
    addRows("No", "Female", failures = 8, passes = 2),
    addRows("Yes", "Female", failures = 6, passes = 4),
    addRows("No", "Male", failures = 7, passes = 3),
    addRows("Yes", "Male", failures = 2, passes = 8)
  )

  fit = stats::glm(Pass ~ Attend * Gender, family = stats::binomial(), data = d)
  promptBlock = suppressWarnings(buildFormattedPromptQuantityBlock(fit))
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(promptBlock, "Pr(Pass = Pass) when Attend = No; Gender = Female on the probability scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "Pr(Pass = Pass) when Attend = Yes; Gender = Female on the probability scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "Pr(Pass = Pass) when Attend = No; Gender = Male on the probability scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "Pr(Pass = Pass) when Attend = Yes; Gender = Male on the probability scale", fixed = TRUE)
  testthat::expect_match(promptBlock, "Odds(Pass = Pass) odds ratio comparing Attend = Yes with Attend = No when Gender = Female", fixed = TRUE)
  testthat::expect_match(promptBlock, "Odds(Pass = Pass) odds ratio comparing Attend = Yes with Attend = No when Gender = Male", fixed = TRUE)
  testthat::expect_match(promptBlock, "Ratio of odds ratios for Odds(Pass = Pass): Attend effect at Gender = Male versus Gender = Female", fixed = TRUE)
  testthat::expect_match(promptBlock, "95% confidence interval", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "coefficient scale", fixed = TRUE)
  testthat::expect_no_match(promptBlock, "AttendYes:GenderMale", fixed = TRUE)

  testthat::expect_match(prompt, "Pr(Pass = Pass) when Attend = No; Gender = Female on the probability scale", fixed = TRUE)
  testthat::expect_match(prompt, "Odds(Pass = Pass) odds ratio comparing Attend = Yes with Attend = No when Gender = Male", fixed = TRUE)
  testthat::expect_match(prompt, "Ratio of odds ratios for Odds(Pass = Pass): Attend effect at Gender = Male versus Gender = Female", fixed = TRUE)
})
