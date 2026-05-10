test_that("developer scoring audit summarises repeated-run stability", {
  fixtures = readDeveloperScoringFixtures()
  audit = summariseDeveloperScoringAudit(fixtures)

  expect_named(
    audit,
    c("runSummary", "exampleSummary", "metricSummary", "unstableMetrics")
  )
  expect_equal(nrow(audit$runSummary), 15)
  expect_equal(nrow(audit$exampleSummary), 5)
  expect_true(all(c("test-SG-1", "test-SG-5") %in% audit$exampleSummary$exampleName))

  sg3 = audit$exampleSummary[audit$exampleSummary$exampleName == "test-SG-3", , drop = FALSE]
  sg4 = audit$exampleSummary[audit$exampleSummary$exampleName == "test-SG-4", , drop = FALSE]
  sg5 = audit$exampleSummary[audit$exampleSummary$exampleName == "test-SG-5", , drop = FALSE]

  expect_equal(sg3$markSpread, 6)
  expect_equal(sg3$lowMarkRunCount, 2)
  expect_true(sg3$unstableExample)

  expect_equal(sg4$markSpread, 5.62)
  expect_equal(sg4$lowMarkRunCount, 2)
  expect_true(sg4$unstableExample)

  expect_equal(sg5$markSpread, 0.38)
  expect_equal(sg5$lowMarkRunCount, 0)
  expect_false(sg5$unstableExample)
})

test_that("developer scoring audit identifies unstable fixture metrics", {
  fixtures = readDeveloperScoringFixtures()
  audit = summariseDeveloperScoringAudit(
    fixtures,
    metricLabels = c(
      "Effect direction correct",
      "Main-effect coverage adequate",
      "Uncertainty handling appropriate"
    )
  )

  expect_true(nrow(audit$metricSummary) > 0)
  expect_true(nrow(audit$unstableMetrics) > 0)

  sg3EffectDirection = audit$metricSummary[
    audit$metricSummary$exampleName == "test-SG-3" &
      audit$metricSummary$metricLabel == "Effect direction correct",
    ,
    drop = FALSE
  ]
  sg4MainEffect = audit$metricSummary[
    audit$metricSummary$exampleName == "test-SG-4" &
      audit$metricSummary$metricLabel == "Main-effect coverage adequate",
    ,
    drop = FALSE
  ]

  expect_equal(sg3EffectDirection$valueSpread, 2)
  expect_true(sg3EffectDirection$unstableMetric)
  expect_equal(sg4MainEffect$valueSpread, 2)
  expect_true(sg4MainEffect$unstableMetric)
})
