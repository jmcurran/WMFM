testthat::test_that("adjustment workflows use deterministic scaffold in final prompt", {
  rawData = data.frame(
    responseValue = c(10, 13, 12, 16, 18, 20, 19, 22),
    primaryA = c(1, 2, 2, 3, 4, 4, 5, 6),
    primaryB = c(2, 1, 3, 2, 4, 3, 5, 4),
    adjustVar = factor(rep(c("levelA", "levelB"), each = 4))
  )

  fit = stats::lm(responseValue ~ primaryA * adjustVar + primaryB, data = rawData)
  attr(fit, "wmfm_adjustment_variables") = "adjustVar"
  attr(fit, "wmfm_research_question") = "How are primaryA and primaryB related to responseValue?"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  adjustmentLevels = levels(rawData$adjustVar)
  coefNames = rownames(stats::coef(summary(fit)))

  testthat::expect_match(prompt, "Deterministic adjustment-aware explanation scaffold:", fixed = TRUE)
  testthat::expect_match(prompt, "Research question: How are primaryA and primaryB related to responseValue\\?", perl = TRUE)
  testthat::expect_match(prompt, "Adjustment variables: adjustVar", fixed = TRUE)
  testthat::expect_match(prompt, "after adjusting for adjustVar", fixed = TRUE)
  testthat::expect_match(prompt, "Do not add new statistical findings.", fixed = TRUE)

  for (levelLabel in adjustmentLevels) {
    testthat::expect_no_match(prompt, levelLabel, fixed = TRUE)
  }

  testthat::expect_no_match(prompt, "At level", ignore.case = TRUE)
  testthat::expect_no_match(prompt, "the fitted mean", ignore.case = TRUE)
  testthat::expect_no_match(prompt, "the contrast", ignore.case = TRUE)
  testthat::expect_no_match(prompt, "confidence interval excludes", ignore.case = TRUE)

  adjustmentCoefs = coefNames[grepl("adjustVar", coefNames, fixed = TRUE)]
  for (coefLabel in adjustmentCoefs) {
    testthat::expect_no_match(prompt, coefLabel, fixed = TRUE)
  }

  testthat::expect_match(
    prompt,
    "Model-structure caveat: The fitted model includes terms involving adjustment variables, so the adjusted comparison is based on that model structure.",
    fixed = TRUE
  )
})

testthat::test_that("simple adjustment workflow includes deterministic adjusted-effect summary", {
  fit = stats::lm(arousal ~ gender + picture, data = s20x::arousal.df)
  attr(fit, "wmfm_adjustment_variables") = "picture"
  attr(fit, "wmfm_research_question") = "Is there a difference in arousal levels for females and males?"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Deterministic adjustment-aware explanation scaffold:", fixed = TRUE)
  testthat::expect_match(prompt, "after adjusting for picture", fixed = TRUE)
  testthat::expect_match(prompt, "Adjusted primary-effect summary:", fixed = TRUE)
  testthat::expect_match(prompt, "95% CI", fixed = TRUE)

  for (levelLabel in levels(s20x::arousal.df$picture)) {
    testthat::expect_no_match(prompt, levelLabel, fixed = TRUE)
  }

  testthat::expect_no_match(prompt, "fitted mean", ignore.case = TRUE)
  testthat::expect_no_match(prompt, "for .* picture", perl = TRUE, ignore.case = TRUE)
  testthat::expect_no_match(prompt, "picture-specific", ignore.case = TRUE)
})

testthat::test_that("interaction adjustment workflow remains high-level only", {
  fit = stats::lm(arousal ~ gender + picture + gender:picture, data = s20x::arousal.df)
  attr(fit, "wmfm_adjustment_variables") = "picture"
  attr(fit, "wmfm_research_question") = "Is there a difference in arousal levels for females and males?"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Model-structure caveat", fixed = TRUE)

  for (levelLabel in levels(s20x::arousal.df$picture)) {
    testthat::expect_no_match(prompt, levelLabel, fixed = TRUE)
  }

  testthat::expect_no_match(prompt, "for .* picture", perl = TRUE, ignore.case = TRUE)
  testthat::expect_no_match(prompt, "at .* level", perl = TRUE, ignore.case = TRUE)
  testthat::expect_no_match(prompt, "interaction .* level", perl = TRUE, ignore.case = TRUE)
})

testthat::test_that("no-adjustment prompt path remains unchanged", {
  rawData = data.frame(
    responseValue = c(5, 6, 7, 8, 9, 10),
    primaryA = c(1, 2, 3, 4, 5, 6),
    primaryB = c(2, 3, 2, 3, 4, 5)
  )

  fit = stats::lm(responseValue ~ primaryA + primaryB, data = rawData)
  attr(fit, "wmfm_adjustment_variables") = character(0)

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_no_match(prompt, "Deterministic adjustment-aware explanation scaffold:", fixed = TRUE)
  testthat::expect_match(prompt, "Explain the model summary below", fixed = TRUE)
})


testthat::test_that("bad-explanation guardrail flags adjustment-level narration", {
  badExplanation = paste(
    "At levelA of adjustVar, the fitted mean is higher and the confidence interval excludes zero.",
    "At levelB of adjustVar, the contrast is smaller, so the interaction changes direction.",
    sep = " "
  )

  hasAdjustmentNarrativeViolations = function(text) {
    patterns = c(
      "\\bat\\s+level\\w+\\b",
      "\\bfitted\\s+mean",
      "\\bcontrast\\b",
      "\\bconfidence\\s+interval\\b",
      "\\binteraction\\b.*\\blevel"
    )

    any(vapply(patterns, function(pattern) {
      grepl(pattern, text, perl = TRUE, ignore.case = TRUE)
    }, logical(1)))
  }

  testthat::expect_true(hasAdjustmentNarrativeViolations(badExplanation))
  testthat::expect_false(hasAdjustmentNarrativeViolations("The model compares primary predictors after adjusting for a control variable."))
})
