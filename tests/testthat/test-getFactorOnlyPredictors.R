test_that("getFactorOnlyPredictors returns factor predictors only", {

  df = data.frame(
    y = rnorm(20),
    f = factor(rep(c("A", "B"), each = 10)),
    x = rnorm(20),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ f + x, data = df)
  mf = model.frame(m)

  res = getFactorOnlyPredictors(m, mf)

  expect_type(res, "character")
  expect_equal(res, "f")
})

test_that("getFactorOnlyPredictors returns multiple factor predictors", {

  df = data.frame(
    y = rnorm(30),
    f1 = factor(rep(c("A", "B", "C"), each = 10)),
    f2 = factor(rep(c("U", "V"), length.out = 30)),
    x = rnorm(30),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ f1 + f2 + x, data = df)
  mf = model.frame(m)

  res = getFactorOnlyPredictors(m, mf)

  expect_setequal(res, c("f1", "f2"))
})

test_that("getFactorOnlyPredictors returns empty character when no factors", {

  df = data.frame(
    y = rnorm(15),
    x1 = rnorm(15),
    x2 = rnorm(15),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ x1 + x2, data = df)
  mf = model.frame(m)

  res = getFactorOnlyPredictors(m, mf)

  expect_identical(res, character(0))
})

test_that("getFactorOnlyPredictors handles intercept-only models", {

  df = data.frame(
    y = rnorm(10),
    f = factor(rep(c("A", "B"), each = 5)),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ 1, data = df)
  mf = model.frame(m)

  res = getFactorOnlyPredictors(m, mf)

  expect_identical(res, character(0))
})
