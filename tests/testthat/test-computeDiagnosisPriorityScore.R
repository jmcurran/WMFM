test_that("computeDiagnosisPriorityScore returns higher score for more systematic disagreement", {
  summaryTable = data.frame(
    disagreementRate = c(1, 0.25),
    meanLlmMinusDet = c(2, 0.5),
    directionConsistency = c(1, 0.5),
    detConstant = c(TRUE, FALSE),
    llmConstant = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  out = computeDiagnosisPriorityScore(summaryTable)

  expect_equal(length(out), 2)
  expect_true(out[[1]] > out[[2]])
})
