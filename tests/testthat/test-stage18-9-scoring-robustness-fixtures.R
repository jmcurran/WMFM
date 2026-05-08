testthat::test_that("stage 18.9 developer scoring exports are available as package fixtures", {
  fixtures = readDeveloperScoringFixtures(stage = "stage18-9")

  testthat::expect_length(fixtures, 5)
  testthat::expect_setequal(
    names(fixtures),
    paste0("test-SG-", 1:5)
  )

  schemas = vapply(fixtures, `[[`, character(1), "schema")
  schemaVersions = vapply(fixtures, `[[`, character(1), "schemaVersion")

  testthat::expect_true(all(schemas == "wmfm-developer-scoring-export"))
  testthat::expect_true(all(schemaVersions == "1.1.0"))
})

testthat::test_that("stage 18.9 fixtures preserve repeated scoring instability audit evidence", {
  fixtures = readDeveloperScoringFixtures(stage = "stage18-9")

  marks = lapply(fixtures, getDeveloperScoringRunMarks)
  markRanges = vapply(
    marks,
    function(x) {
      max(x) - min(x)
    },
    numeric(1)
  )

  testthat::expect_equal(marks[["test-SG-3"]], c(10, 4, 4))
  testthat::expect_equal(marks[["test-SG-4"]], c(9.62, 4, 4))
  testthat::expect_true(markRanges[["test-SG-3"]] >= 6)
  testthat::expect_true(markRanges[["test-SG-4"]] >= 5)
  testthat::expect_true(markRanges[["test-SG-2"]] == 0)
})

testthat::test_that("stage 18.9 fixtures identify the metrics responsible for scoring cliffs", {
  fixtures = readDeveloperScoringFixtures(stage = "stage18-9")
  sg3LowRun = fixtures[["test-SG-3"]]$repeated$runs[[2]]
  sg4LowRun = fixtures[["test-SG-4"]]$repeated$runs[[2]]

  cliffLabels = c(
    "Effect direction correct",
    "Main-effect coverage adequate"
  )

  sg3Values = vapply(
    cliffLabels,
    function(label) {
      getDeveloperScoringMetricValue(sg3LowRun, label)
    },
    numeric(1)
  )

  sg4Values = vapply(
    cliffLabels,
    function(label) {
      getDeveloperScoringMetricValue(sg4LowRun, label)
    },
    numeric(1)
  )

  testthat::expect_equal(unname(sg3Values), c(0, 0))
  testthat::expect_equal(unname(sg4Values), c(0, 0))

  testthat::expect_equal(
    getDeveloperScoringMetricValue(sg3LowRun, "Uncertainty handling appropriate"),
    2
  )
  testthat::expect_equal(
    getDeveloperScoringMetricValue(sg4LowRun, "Uncertainty handling appropriate"),
    2
  )
})
