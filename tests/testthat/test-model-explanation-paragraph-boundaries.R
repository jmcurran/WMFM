testthat::test_that("post-processing preserves deterministic follow-up paragraph breaks", {
  input = paste(
    "Main research-question answer.",
    "For the follow-up question, using Attend = Yes, Test = 10, WMFM predicts an expected Exam of 49.88. For an individual outcome with these predictor values, the 95% prediction interval is 26.94 to 72.82.",
    sep = "\n\n"
  )

  out = postProcessExplanationText(input)

  testthat::expect_match(
    out,
    "Main research-question answer\\.\\n\\nFor the follow-up question",
    perl = TRUE
  )
  testthat::expect_match(out, "49.88", fixed = TRUE)
  testthat::expect_match(out, "26.94", fixed = TRUE)
  testthat::expect_match(out, "72.82", fixed = TRUE)
})

testthat::test_that("post-processing normalises horizontal whitespace without flattening paragraphs", {
  input = paste0(
    "Main answer.   ",
    "\n   \n",
    "For the follow-up question, using Attend = Yes, Test = 10,   WMFM predicts an expected Exam of 49.88."
  )

  out = postProcessExplanationText(input)

  testthat::expect_match(
    out,
    "Main answer\\.\\n\\nFor the follow-up question",
    perl = TRUE
  )
  testthat::expect_no_match(out, " {2,}", perl = TRUE)
})


testthat::test_that("post-processing standardises compact confidence interval formatting", {
  narrowNonBreakingSpace = intToUtf8(0x202F)
  enDash = intToUtf8(0x2013)
  input = paste(
    paste0("The expected mark is around 55 (95", narrowNonBreakingSpace, "%", narrowNonBreakingSpace, "CI", narrowNonBreakingSpace, "53-58)."),
    "Each point adds 3.5 marks (95% CI 3-4)."
  )

  out = postProcessExplanationText(input)

  testthat::expect_match(out, paste0("95% confidence interval: \\[53", enDash, "58\\]"), perl = TRUE)
  testthat::expect_match(out, paste0("95% confidence interval: \\[3", enDash, "4\\]"), perl = TRUE)
  testthat::expect_no_match(out, "95[[:space:]]*%[[:space:]]*CI", perl = TRUE)
})


testthat::test_that("numeric-token preservation treats hyphenated ranges as ranges", {
  before = "The interval is 53-58."
  after = paste0("The interval is 53", intToUtf8(0x2013), "58.")

  testthat::expect_true(postProcessPreservesNumericTokens(before, after))
})
