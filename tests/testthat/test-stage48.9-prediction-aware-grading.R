testthat::test_that("negated guarantee language is not overclaiming", {
  record = buildWmfmRunRecord(
    runId = 1L,
    exampleName = "course grading",
    package = "s20x",
    modelType = "lm",
    formula = "Exam ~ Attend + Test",
    equationsText = "Exam = 6.6 + 8 Attend + 3.5 Test",
    researchQuestion = "Will I do well?",
    explanationText = paste(
      "The predicted exam mark is 67.5 with a 95% prediction interval from 44.6 to 90.5.",
      "There is no guarantee that the student will do well."
    )
  )

  testthat::expect_false(record$overclaimDetected)
})

testthat::test_that("prediction and expected-response evidence is recognised", {
  evidence = extractWmfmSemanticEvidence(
    explanationText = paste(
      "The predicted exam mark is 67.5 with a 95% prediction interval from 44.6 to 90.5.",
      "In comparison, the expected mark is 67.5 with a 95% confidence interval from 64.81 to 70.19.",
      "The confidence interval is narrower because it concerns the average mark, not a prediction for a new student."
    ),
    modelInfo = list(formula = "Exam ~ Attend + Test", researchQuestion = "Will I do well?")
  )

  testthat::expect_true(evidence$comparisonMentioned)
  testthat::expect_true(evidence$individualPredictionMentioned)
  testthat::expect_true(evidence$predictionIntervalMentioned)
  testthat::expect_true(evidence$expectedResponseMentioned)
  testthat::expect_true(evidence$confidenceIntervalForMeanMentioned)
  testthat::expect_true(evidence$predictionMeanDistinctionMentioned)
  testthat::expect_false(is.na(evidence$effectMagnitude))
})

testthat::test_that("course grading reproduction example is developer only", {
  visible = listWMFMExamples(package = "WMFM")
  developer = listWMFMExamples(package = "WMFM", includeTestExamples = TRUE)

  testthat::expect_false("test-Course Explanation Grading" %in% visible)
  testthat::expect_true("test-Course Explanation Grading" %in% developer)

  info = loadExampleSpec("test-Course Explanation Grading", package = "WMFM")
  testthat::expect_identical(info$spec$formula, "Exam ~ Attend + Test")
  testthat::expect_match(info$researchQuestion, "Will I do well", fixed = TRUE)
})
