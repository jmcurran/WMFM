test_that("fillMissingPredictors adds missing predictors using training frame values", {

  set.seed(1)

  df = data.frame(
    y = rnorm(10),
    x = rnorm(10),
    Exam = factor(rep(c("A", "B"), each = 5))
  )

  m = lm(y ~ x + Exam, data = df)

  newData = data.frame(x = seq(min(df$x), max(df$x), length.out = 3))

  out = fillMissingPredictors(m, newData)

  expect_true("Exam" %in% names(out))
  expect_equal(nrow(out), nrow(newData))

  # Uses first row of training frame for missing vars
  mf = model.frame(m)
  expect_equal(as.character(out$Exam), rep(as.character(mf$Exam[1]), nrow(newData)))

  # Factor levels preserved
  expect_true(is.factor(out$Exam))
  expect_equal(levels(out$Exam), levels(mf$Exam))
})

test_that("fillMissingPredictors does not overwrite columns already present in newData", {

  df = data.frame(
    y = rnorm(10),
    x = rnorm(10),
    Exam = factor(rep(c("A", "B"), each = 5))
  )

  m = lm(y ~ x + Exam, data = df)

  # Provide Exam explicitly (different from training first row)
  newData = data.frame(
    x = c(0, 1, 2),
    Exam = factor(c("B", "B", "B"), levels = c("A", "B"))
  )

  out = fillMissingPredictors(m, newData)

  expect_equal(as.character(out$Exam), rep("B", 3))
  # Still enforces training levels (should remain A,B)
  mf = model.frame(m)
  expect_equal(levels(out$Exam), levels(mf$Exam))
})

test_that("fillMissingPredictors re-levels factor columns to match training levels", {

  df = data.frame(
    y = rnorm(10),
    x = rnorm(10),
    Exam = factor(rep(c("A", "B"), each = 5))
  )

  m = lm(y ~ x + Exam, data = df)

  # Exam provided but with wrong level set/order
  newData = data.frame(
    x = c(0, 1, 2),
    Exam = factor(c("B", "B", "B"), levels = c("B", "A", "C"))
  )

  out = fillMissingPredictors(m, newData)

  mf = model.frame(m)
  expect_equal(levels(out$Exam), levels(mf$Exam))
  expect_equal(as.character(out$Exam), rep("B", 3))
})

test_that("fillMissingPredictors is a no-op for intercept-only models", {

  df = data.frame(
    y = rnorm(10)
  )

  m = lm(y ~ 1, data = df)

  newData = data.frame(dummy = 1:3)

  out = fillMissingPredictors(m, newData)

  # Should not add anything (no RHS vars)
  expect_equal(names(out), names(newData))
  expect_equal(out, newData)
})
