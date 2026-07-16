test_that("ordered factors become nominal factors", {
  orderedValue = ordered(c("Fair", "Good", "Very Good"))

  result = coerceWmfmFactor(orderedValue)

  expect_true(is.factor(result))
  expect_false(is.ordered(result))
  expect_identical(as.character(result), as.character(orderedValue))
  expect_equal(
    unname(contrasts(result)),
    unname(contr.treatment(nlevels(result)))
  )
})

test_that("ordinary factors retain their level order", {
  factorValue = factor(c("B", "A", "B"), levels = c("B", "A"))

  result = coerceWmfmFactor(factorValue)

  expect_identical(result, factorValue)
})
