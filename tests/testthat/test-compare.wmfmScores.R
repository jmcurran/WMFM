test_that("compare.wmfmScores performs a within-object comparison", {
  x = makeFakeWmfmScores(makeLongScoreDf())

  out = compare(x)

  expect_s3_class(out, "wmfmScoreComparison")
  expect_equal(out$source, "within_object")
  expect_equal(out$leftMethod, "deterministic")
  expect_equal(out$rightMethod, "llm")
  expect_true(nrow(out$pairData) > 0)
})

test_that("compare.wmfmScores respects explicit within-object methods", {
  x = makeFakeWmfmScores(makeLongScoreDf())

  out = compare(x, xMethod = "llm", yMethod = "deterministic")

  expect_equal(out$leftMethod, "llm")
  expect_equal(out$rightMethod, "deterministic")
})

test_that("compare.wmfmScores performs a between-object comparison", {
  longDf = makeLongScoreDf()
  x = makeFakeWmfmScores(subset(longDf, method == "deterministic"), methods = "deterministic")
  y = makeFakeWmfmScores(subset(longDf, method == "llm"), methods = "llm")

  out = compare(x, y)

  expect_s3_class(out, "wmfmScoreComparison")
  expect_equal(out$source, "between_objects")
  expect_equal(out$leftMethod, "deterministic")
  expect_equal(out$rightMethod, "llm")
})

test_that("compare.wmfmScores errors when within-object comparison is ambiguous", {
  longDf = makeLongScoreDf()
  longDf$method[longDf$method == "llm"] = "rubric"
  x = makeFakeWmfmScores(longDf, methods = c("deterministic", "rubric"))

  out = compare(x)
  expect_equal(out$leftMethod, "deterministic")
  expect_equal(out$rightMethod, "rubric")
})

test_that("compare.wmfmScores errors on invalid method requests", {
  x = makeFakeWmfmScores(makeLongScoreDf())

  expect_error(compare(x, xMethod = "banana"), "xMethod")

  y = makeFakeWmfmScores(
    subset(makeLongScoreDf(), method == "llm"),
    methods = "llm"
  )

  expect_error(
    compare(x, y, xMethod = "deterministic", yMethod = "banana"),
    "Requested method not present"
  )
})
