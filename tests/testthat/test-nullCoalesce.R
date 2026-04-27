test_that("%||% returns fallback only when x is NULL", {

  expect_identical(NULL %||% 123, 123)
  expect_identical(NULL %||% "x", "x")

  expect_identical(0 %||% 123, 0)
  expect_identical(FALSE %||% TRUE, FALSE)

  expect_identical(NA %||% 5, NA)
  expect_identical(NA_real_ %||% 5, NA_real_)

  expect_identical(character(0) %||% "fallback", character(0))
  expect_identical(integer(0) %||% 1L, integer(0))
  expect_identical(list() %||% "fallback", list())
})

test_that("%||% returns x unchanged when x is not NULL", {

  x = list(a = 1)
  y = list(b = 2)

  out = x %||% y
  expect_identical(out, x)
})
