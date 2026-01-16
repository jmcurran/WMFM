test_that("buildContrastNewData builds one row per target level", {

  df = data.frame(
    y = rnorm(12),
    a = factor(rep(c("L1", "L2", "L3"), each = 4)),
    b = factor(rep(c("U", "V"), length.out = 12)),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ a + b, data = df)
  mf = model.frame(m)

  out = buildContrastNewData(
    mf = mf,
    factorPreds = c("a", "b"),
    targetFactor = "a",
    condValues = list(b = "U")
  )

  expect_true(is.list(out))
  expect_named(out, c("newData", "targetLevels"))

  expect_identical(out$targetLevels, levels(mf$a))
  expect_equal(nrow(out$newData), length(levels(mf$a)))
  expect_true(is.factor(out$newData$a))
  expect_true(is.factor(out$newData$b))

  expect_identical(as.character(out$newData$b), rep("U", length(levels(mf$a))))
  expect_identical(levels(out$newData$b), levels(mf$b))
})

test_that("buildContrastNewData errors when condValues missing for other factors", {

  df = data.frame(
    y = rnorm(10),
    a = factor(rep(c("L1", "L2"), each = 5)),
    b = factor(rep(c("U", "V"), length.out = 10)),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ a + b, data = df)
  mf = model.frame(m)

  expect_error(
    buildContrastNewData(
      mf = mf,
      factorPreds = c("a", "b"),
      targetFactor = "a",
      condValues = list()
    ),
    "Missing condValues"
  )
})

test_that("buildContrastNewData errors when condValues is not a valid level", {

  df = data.frame(
    y = rnorm(10),
    a = factor(rep(c("L1", "L2"), each = 5)),
    b = factor(rep(c("U", "V"), length.out = 10)),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ a + b, data = df)
  mf = model.frame(m)

  expect_error(
    buildContrastNewData(
      mf = mf,
      factorPreds = c("a", "b"),
      targetFactor = "a",
      condValues = list(b = "NOT_A_LEVEL")
    ),
    "not a level"
  )
})

test_that("buildContrastNewData errors when targetFactor is not in factorPreds", {

  df = data.frame(
    y = rnorm(10),
    a = factor(rep(c("L1", "L2"), each = 5)),
    b = factor(rep(c("U", "V"), length.out = 10)),
    stringsAsFactors = FALSE
  )

  m = lm(y ~ a + b, data = df)
  mf = model.frame(m)

  expect_error(
    buildContrastNewData(
      mf = mf,
      factorPreds = "b",
      targetFactor = "a",
      condValues = list(b = "U")
    ),
    "targetFactor must be an element"
  )
})
