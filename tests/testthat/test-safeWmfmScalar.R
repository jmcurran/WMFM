testthat::test_that("safeWmfmScalar handles common scalar cases", {
  testthat::expect_identical(safeWmfmScalar(NULL), "NA")
  testthat::expect_identical(safeWmfmScalar(character(0)), "NA")
  testthat::expect_identical(safeWmfmScalar(" abc "), "abc")
  testthat::expect_identical(safeWmfmScalar("a\nb", singleLine = TRUE), "a b")
})

testthat::test_that("safeWmfmScalar handles factors and nested lists", {
  x = factor("Yes", levels = c("No", "Yes"))
  testthat::expect_identical(safeWmfmScalar(x), "Yes")
  testthat::expect_identical(safeWmfmScalar(list(list("hello"))), "hello")
})

testthat::test_that("safeWmfmScalar validates arguments", {
  testthat::expect_error(
    safeWmfmScalar("x", naString = NA_character_),
    "naString"
  )
  testthat::expect_error(
    safeWmfmScalar("x", trim = NA),
    "trim"
  )
  testthat::expect_error(
    safeWmfmScalar("x", singleLine = NA),
    "singleLine"
  )
})
