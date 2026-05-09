test_that("calibrated developer scoring fixtures avoid repeated-scoring cliffs", {
  fixtures = readDeveloperScoringFixtures(stage = "stage19-3-1")
  audit = summariseDeveloperScoringAudit(fixtures)

  expect_equal(nrow(audit$runSummary), 15)
  expect_equal(nrow(audit$exampleSummary), 5)
  expect_false(any(audit$exampleSummary$unstableExample))

  sg3 = audit$exampleSummary[
    audit$exampleSummary$exampleName == "test-SG-3",
    ,
    drop = FALSE
  ]
  sg4 = audit$exampleSummary[
    audit$exampleSummary$exampleName == "test-SG-4",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(sg3), 1)
  expect_equal(nrow(sg4), 1)
  expect_lte(sg3$markSpread, 1)
  expect_lte(sg4$markSpread, 1)
  expect_equal(sg3$lowMarkRunCount, 0)
  expect_equal(sg4$lowMarkRunCount, 0)
})

test_that("calibrated developer scoring metrics do not reintroduce SG-3 or SG-4 cliffs", {
  fixtures = readDeveloperScoringFixtures(stage = "stage19-3-1")
  audit = summariseDeveloperScoringAudit(fixtures)

  severeExamples = audit$unstableMetrics$exampleName %in% c("test-SG-3", "test-SG-4")
  expect_false(any(severeExamples))

  sg5 = audit$exampleSummary[
    audit$exampleSummary$exampleName == "test-SG-5",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(sg5), 1)
  expect_lte(sg5$markSpread, 1)
  expect_equal(sg5$lowMarkRunCount, 0)
})
