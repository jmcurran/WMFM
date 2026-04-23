testthat::test_that("validateWmfmParsedScores validates good input", {
  out = validateWmfmParsedScores(makeValidParsedScores())

  testthat::expect_identical(out$effectDirectionCorrect, 2L)
  testthat::expect_identical(out$fatalFlawDetected, FALSE)
  testthat::expect_true(is.list(out$fieldReasons))
})

testthat::test_that("validateWmfmParsedScores rejects bad rubric values", {
  x = makeValidParsedScores()
  x$effectDirectionCorrect = 4

  testthat::expect_error(
    validateWmfmParsedScores(x),
    "effectDirectionCorrect"
  )
})

testthat::test_that("validateWmfmParsedScores rejects missing reason fields", {
  x = makeValidParsedScores()
  x$fieldReasons$clarityAdequate = NULL

  testthat::expect_error(
    validateWmfmParsedScores(x),
    "fieldReasons"
  )
})
