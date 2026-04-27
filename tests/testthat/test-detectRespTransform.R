test_that("detectRespTransform detects supported transforms", {
  expect_identical(detectRespTransform("log(y)"), "log")
  expect_identical(detectRespTransform(" log1p(count) "), "log1p")
  expect_identical(detectRespTransform("sqrt(x)"), "sqrt")
})

test_that("detectRespTransform ignores whitespace", {
  expect_identical(detectRespTransform(" log ( y ) "), "log")
  expect_identical(detectRespTransform("  log1p ( x ) "), "log1p")
})

test_that("detectRespTransform returns NULL when no transform detected", {
  expect_null(detectRespTransform(NULL))
  expect_null(detectRespTransform(""))
  expect_null(detectRespTransform("y"))
  expect_null(detectRespTransform("exp(y)"))
})
