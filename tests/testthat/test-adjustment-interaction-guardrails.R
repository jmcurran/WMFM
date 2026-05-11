testthat::test_that("interaction labels flag adjustment-variable involvement", {
  testthat::expect_true(termInvolvesAdjustmentVariable("x:z", "z"))
  testthat::expect_true(termInvolvesAdjustmentVariable("x:z:w", c("q", "w")))
  testthat::expect_false(termInvolvesAdjustmentVariable("x:z", "age"))
})

testthat::test_that("term-evidence prompt labels interactions with adjustment variables explicitly", {
  rawData = data.frame(
    y = c(1, 2, 3, 4, 5, 6, 7, 8),
    x = c(1, 1, 2, 2, 3, 3, 4, 4),
    z = c(1, 2, 1, 2, 1, 2, 1, 2)
  )

  fittedModel = stats::lm(y ~ x * z, data = rawData)
  attr(fittedModel, "wmfm_adjustment_variables") = "z"

  block = buildLmTermEvidencePromptBlock(fittedModel)
  testthat::expect_match(
    block,
    "x:z: .*\\(interaction, interaction involving adjustment variable\\)",
    perl = TRUE
  )
})

testthat::test_that("term-evidence prompt keeps non-adjustment interactions on primary role label", {
  rawData = data.frame(
    y = c(2, 3, 3, 5, 6, 7, 9, 10),
    x = c(1, 1, 2, 2, 3, 3, 4, 4),
    z = c(1, 2, 1, 2, 1, 2, 1, 2)
  )

  fittedModel = stats::lm(y ~ x * z, data = rawData)
  attr(fittedModel, "wmfm_adjustment_variables") = "w"

  block = buildLmTermEvidencePromptBlock(fittedModel)
  testthat::expect_match(block, "x:z: .*\\(interaction, primary predictor\\)", perl = TRUE)
})
