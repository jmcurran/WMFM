testthat::test_that("ensureAnchoredFactorComparisonText inserts omitted anchored comparison", {
  dat = data.frame(
    Exam = c(38, 42, 47, 52, 46, 50, 55, 60),
    Attend = factor(c("No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")),
    Test = c(8, 10, 12, 14, 8, 10, 12, 14)
  )

  fit = stats::lm(Exam ~ Attend + Test, data = dat)
  text = paste(
    "Each additional point on the mid-term test is associated with a rise.",
    "For the follow-up question, using Attend = Yes, Test = 10, WMFM predicts an expected Exam of 49.88.",
    sep = "\n\n"
  )

  out = ensureAnchoredFactorComparisonText(text = text, model = fit)

  testthat::expect_match(out, "At Test = 11", fixed = TRUE)
  testthat::expect_match(out, "Attend = No", fixed = TRUE)
  testthat::expect_match(out, "Attend = Yes", fixed = TRUE)
  testthat::expect_match(out, "difference of about 8", fixed = TRUE)
  testthat::expect_match(
    out,
    "difference of about 8 points associated with Attend = Yes.\n\nFor the follow-up question"
  )
})

testthat::test_that("ensureAnchoredFactorComparisonText leaves existing anchored comparison alone", {
  dat = data.frame(
    Exam = c(38, 42, 47, 52, 46, 50, 55, 60),
    Attend = factor(c("No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")),
    Test = c(8, 10, 12, 14, 8, 10, 12, 14)
  )

  fit = stats::lm(Exam ~ Attend + Test, data = dat)
  sentence = buildAnchoredFactorComparisonSentence(fit)
  out = ensureAnchoredFactorComparisonText(text = sentence, model = fit)

  testthat::expect_identical(out, sentence)
})
