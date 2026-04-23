testthat::test_that("newWmfmScores creates an empty scores object aligned to runs", {
  x = makeFakeWmfmRuns()

  out = newWmfmScores(
    x = x,
    methods = c("deterministic", "llm")
  )

  testthat::expect_s3_class(out, "wmfmScores")
  testthat::expect_identical(out$methods, c("deterministic", "llm"))
  testthat::expect_identical(out$runIds, c(1L, 2L, 3L))
  testthat::expect_length(out$scores$deterministic, 3)
  testthat::expect_length(out$scores$llm, 3)
  testthat::expect_identical(out$meta$nRuns, 3L)
})

testthat::test_that("newWmfmScores rejects unsupported methods", {
  x = makeFakeWmfmRuns()

  testthat::expect_error(
    newWmfmScores(x = x, methods = c("deterministic", "banana")),
    "Unsupported scoring method"
  )
})
