test_that("fmt3 returns character output and preserves length", {
  x = c(0.001234, 12.3456, 12345.6, NA_real_)
  out = fmt3(x)

  expect_type(out, "character")
  expect_equal(length(out), length(x))
})

test_that("fmt3 uses approximately three significant figures", {
  expect_identical(fmt3(12.3456), "12.3")
  expect_identical(fmt3(0.001234), "0.00123")
})
