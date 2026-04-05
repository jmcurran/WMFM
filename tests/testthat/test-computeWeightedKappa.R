test_that("computeWeightedKappa returns 1 for perfect agreement", {
  expect_equal(
    computeWeightedKappa(c(1L, 2L, 3L), c(1L, 2L, 3L)),
    1
  )
})

test_that("computeWeightedKappa returns NA for empty complete pairs", {
  expect_true(is.na(computeWeightedKappa(c(NA, NA), c(NA, NA))))
})

test_that("computeWeightedKappa is less than 1 when agreement is imperfect", {
  result = computeWeightedKappa(c(1L, 2L, 3L), c(1L, 3L, 2L))
  expect_true(is.finite(result))
  expect_lt(result, 1)
})
