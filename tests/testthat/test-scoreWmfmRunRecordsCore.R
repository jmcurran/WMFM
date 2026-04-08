test_that("scoreWmfmRunRecordsCore matches repeated-run scoring for one record", {
  m = makeOfflineWmfmModel()

  record = buildWmfmGradeRunRecord(
    x = m,
    explanation = paste(
      "Higher x is associated with higher expected y.",
      "The fitted relationship is descriptive rather than causal."
    )
  )

  recordDf = as.data.frame(record, stringsAsFactors = FALSE)

  scoredCore = scoreWmfmRunRecordsCore(
    runsDf = recordDf,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    penaliseDuplicates = FALSE,
    fatalFlawCap = 40,
    passThreshold = 65
  )

  scoredRepeated = scoreWmfmRepeatedRuns(
    runsDf = recordDf,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    penaliseDuplicates = FALSE,
    fatalFlawCap = 40,
    passThreshold = 65
  )

  expect_equal(scoredCore, scoredRepeated)
})


test_that("score.wmfmGrade uses shared scoring core for student and model answers", {
  m = makeOfflineWmfmModel()

  g = grade(
    m,
    explanation = "Higher x tends to be associated with higher y.",
    modelAnswer = paste(
      "Higher x tends to be associated with higher expected y.",
      "This is an association rather than a causal claim."
    ),
    score = FALSE
  )

  g = score(g)

  expectedStudent = scoreWmfmRunRecordsCore(
    runsDf = as.data.frame(g$records$student, stringsAsFactors = FALSE),
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    penaliseDuplicates = FALSE,
    fatalFlawCap = 40,
    passThreshold = 65
  )

  expectedModelAnswer = scoreWmfmRunRecordsCore(
    runsDf = as.data.frame(g$records$modelAnswer, stringsAsFactors = FALSE),
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    penaliseDuplicates = FALSE,
    fatalFlawCap = 40,
    passThreshold = 65
  )

  expect_equal(g$scores$student, expectedStudent)
  expect_equal(g$scores$modelAnswer, expectedModelAnswer)
})
