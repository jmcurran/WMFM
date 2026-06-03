test_that("addDerivedVariableToData records recognised transformations", {
  df = data.frame(price = c(10, 20, 40), carat = c(1, 2, 4))

  logRes = addDerivedVariableToData(df, "logPrice = log(price)")
  expect_true(logRes$ok)
  expect_s3_class(logRes$transformation, "wmfmVariableTransformation")
  expect_equal(logRes$transformation$variable, "logPrice")
  expect_equal(logRes$transformation$rhs, "log(price)")
  expect_equal(logRes$transformation$sourceVariables, "price")
  expect_equal(logRes$transformation$transformationType, "log")
  expect_equal(logRes$transformation$inverseType, "exp")

  log10Res = addDerivedVariableToData(df, "log10Price = log10(price)")
  expect_true(log10Res$ok)
  expect_equal(log10Res$transformation$transformationType, "log10")
  expect_equal(log10Res$transformation$inverseType, "power10")

  sqrtRes = addDerivedVariableToData(df, "sqrtPrice = sqrt(price)")
  expect_true(sqrtRes$ok)
  expect_equal(sqrtRes$transformation$transformationType, "sqrt")
  expect_equal(sqrtRes$transformation$inverseType, "square")
})

test_that("addDerivedVariableToData records simple arithmetic transformations", {
  df = data.frame(price = c(10, 20, 40), carat = c(1, 2, 4))

  scaledRes = addDerivedVariableToData(df, "priceThousands = price / 1000")
  expect_true(scaledRes$ok)
  expect_equal(scaledRes$transformation$sourceVariables, "price")
  expect_equal(scaledRes$transformation$transformationType, "divideConstant")
  expect_equal(scaledRes$transformation$inverseType, "multiplyConstant")
  expect_equal(scaledRes$transformation$transformationParameters$constant, 1000)

  shiftedRes = addDerivedVariableToData(df, "pricePlusFive = price + 5")
  expect_true(shiftedRes$ok)
  expect_equal(shiftedRes$transformation$sourceVariables, "price")
  expect_equal(shiftedRes$transformation$transformationType, "addConstant")
  expect_equal(shiftedRes$transformation$inverseType, "subtractConstant")
  expect_equal(shiftedRes$transformation$transformationParameters$constant, 5)
})

test_that("failed derived-variable creation does not return transformation metadata", {
  df = data.frame(price = c(10, 20, 40))

  res = addDerivedVariableToData(df, "badVar = unknownColumn + 1")

  expect_false(res$ok)
  expect_null(res$transformation)
})
