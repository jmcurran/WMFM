test_that("buildWmfmScoreComparison returns structured comparison object", {
  longDf = makeLongScoreDf()
  leftDf = subset(longDf, method == "deterministic")
  rightDf = subset(longDf, method == "llm")

  out = buildWmfmScoreComparison(
    leftDf = leftDf,
    rightDf = rightDf,
    leftMethod = "deterministic",
    rightMethod = "llm",
    sourceLabel = "within_object"
  )

  expect_s3_class(out, "wmfmScoreComparison")
  expect_equal(out$source, "within_object")
  expect_equal(out$leftMethod, "deterministic")
  expect_equal(out$rightMethod, "llm")
  expect_s3_class(out$binaryAgreement, "data.frame")
  expect_s3_class(out$ordinalAgreement, "data.frame")
  expect_s3_class(out$continuousAgreement, "data.frame")
  expect_s3_class(out$pairData, "data.frame")
  expect_s3_class(out$pairedOverallScores, "data.frame")
  expect_type(out$overallSummary, "list")
})

test_that("buildWmfmScoreComparison computes overall summary from overallScore", {
  longDf = makeLongScoreDf()
  leftDf = subset(longDf, method == "deterministic")
  rightDf = subset(longDf, method == "llm")

  out = buildWmfmScoreComparison(
    leftDf = leftDf,
    rightDf = rightDf,
    leftMethod = "deterministic",
    rightMethod = "llm",
    sourceLabel = "within_object"
  )

  expectedDiff = c(0.2, -0.2, 0.2)

  expect_equal(out$pairedOverallScores$differenceOverallScore, expectedDiff)
  expect_equal(out$overallSummary$meanDifferenceRightMinusLeft, mean(expectedDiff))
})
