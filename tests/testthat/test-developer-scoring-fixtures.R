test_that("developer scoring fixtures load repeated scoring examples", {
  fixtures = readDeveloperScoringFixtures()

  expect_named(
    fixtures,
    c("test-SG-1", "test-SG-2", "test-SG-3", "test-SG-4", "test-SG-5"),
    ignore.order = TRUE
  )

  expect_equal(getDeveloperScoringRunMarks(fixtures[["test-SG-3"]]), c(10, 4, 4))
  expect_equal(getDeveloperScoringRunMarks(fixtures[["test-SG-4"]]), c(9.62, 4, 4))
})

test_that("developer scoring fixtures identify low-scoring cliff metrics", {
  fixtures = readDeveloperScoringFixtures()
  sg3LowRun = fixtures[["test-SG-3"]]$repeated$runs[[2]]
  sg4LowRun = fixtures[["test-SG-4"]]$repeated$runs[[2]]

  sg3Values = vapply(
    c("Effect direction correct", "Main-effect coverage adequate"),
    function(label) {
      getDeveloperScoringMetricValue(sg3LowRun, label)
    },
    numeric(1)
  )
  sg4Values = vapply(
    c("Effect direction correct", "Main-effect coverage adequate"),
    function(label) {
      getDeveloperScoringMetricValue(sg4LowRun, label)
    },
    numeric(1)
  )

  expect_equal(unname(sg3Values), c(0, 0))
  expect_equal(unname(sg4Values), c(0, 0))
})
