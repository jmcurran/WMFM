testthat::test_that("buildWmfmRunRecord builds a raw run record without score fields", {
  out = buildWmfmRunRecord(
    runId = 1L,
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    formula = "Exam ~ Attend",
    equationsText = "Exam = 6.62 + 3.52 * Attend",
    explanationText = paste(
      "Attendance is associated with higher exam scores.",
      "Compared with the baseline, scores increase by about 3.5 points."
    ),
    interactionTerms = character(0),
    interactionMinPValue = NA_real_,
    interactionAlpha = 0.05
  )

  testthat::expect_identical(out$runId, 1L)
  testthat::expect_identical(out$exampleName, "Course")
  testthat::expect_identical(out$package, "WMFM")
  testthat::expect_identical(out$modelType, "lm")
  testthat::expect_identical(out$hasInteractionTerms, FALSE)
  testthat::expect_identical(out$nInteractionTerms, 0L)
  testthat::expect_true(is.logical(out$comparisonLanguageMention))
  testthat::expect_true(is.numeric(out$wordCount))
  testthat::expect_true(is.numeric(out$sentenceCount))

  testthat::expect_false(any(c(
    "effectDirectionCorrect",
    "overallScore",
    "overallPass",
    "llmScored"
  ) %in% names(out)))
})

testthat::test_that("buildWmfmRunRecord records interaction metadata separately", {
  out = buildWmfmRunRecord(
    runId = 2L,
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    formula = "Exam ~ Attend * Group",
    equationsText = "Exam = 6.62 + 3.52 * Attend + Group + Attend:Group",
    explanationText = "The effect varies by group and is steeper for Group B.",
    interactionTerms = c("Attend:Group"),
    interactionMinPValue = 0.012,
    interactionAlpha = 0.05
  )

  testthat::expect_identical(out$hasInteractionTerms, TRUE)
  testthat::expect_identical(out$nInteractionTerms, 1L)
  testthat::expect_match(out$interactionTerms, "Attend:Group")
  testthat::expect_identical(out$interactionSubstantiveClaim, "difference_claimed_strongly")
})
