testthat::test_that("unit-change phrasing cleanup is predictor-name agnostic", {
  text = "A one-depth increase multiplies the expected count by 0.8. One-depth rise is the unit-change phrase."

  out = postProcessUnitChangePhrasing(text)

  testthat::expect_match(out, "If depth increases by one unit", fixed = TRUE)
  testthat::expect_match(out, "an increase of one unit in depth", fixed = TRUE)
})

testthat::test_that("unit-change classifier accepts named units without hard-coded example nouns", {
  payload = classifyModelFollowupQuestion("Can you express the effect for a 0.1 depth increase?")

  testthat::expect_identical(payload$category, "unit_change_request")
  testthat::expect_true(payload$supported)
  testthat::expect_equal(payload$unitChangeValues, 0.1)
})

testthat::test_that("semantic factor matching uses model levels rather than domain synonyms", {
  levels = c("GroupA", "GroupB")

  testthat::expect_identical(
    matchSemanticNamedFactorLevel("Group", levels, "What would we expect for GroupB?"),
    "GroupB"
  )
  testthat::expect_identical(
    matchSemanticNamedFactorLevel("Group", levels, "What would we expect for the second group?"),
    character(0)
  )
})
