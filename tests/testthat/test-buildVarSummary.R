test_that("buildVarSummary returns expected structure and rows", {

  df = data.frame(
    a = factor(c("L1", "L2", "L1")),
    b = c(1, NA, 3),
    c = c("x", "y", "x"),
    d = c(TRUE, NA, FALSE),
    stringsAsFactors = FALSE
  )

  out = buildVarSummary(df)

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), ncol(df))
  expect_named(out, c("var", "class", "missing", "details"))

  expect_setequal(out$var, names(df))
})

test_that("buildVarSummary describes factor levels", {

  df = data.frame(
    a = factor(c("L1", "L2", "L1")),
    stringsAsFactors = FALSE
  )

  out = buildVarSummary(df)

  expect_equal(out$class[1], "factor")
  expect_match(out$details[1], "^Levels: ")
  expect_match(out$details[1], "L1")
  expect_match(out$details[1], "L2")
})

test_that("buildVarSummary describes numeric range and all-missing numeric", {

  df1 = data.frame(
    x = c(1, NA, 3),
    stringsAsFactors = FALSE
  )
  out1 = buildVarSummary(df1)
  expect_match(out1$details[1], "^Range: ")
  expect_match(out1$details[1], "1")
  expect_match(out1$details[1], "3")

  df2 = data.frame(
    x = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  out2 = buildVarSummary(df2)
  expect_identical(out2$details[1], "Range: (all missing)")
})

test_that("buildVarSummary truncates factor levels and unique values", {

  df = data.frame(
    f = factor(paste0("L", 1:10)),
    s = as.character(paste0("v", 1:10)),
    stringsAsFactors = FALSE
  )

  out = buildVarSummary(df, maxLevels = 3, maxUnique = 4)

  fRow = out[out$var == "f", , drop = FALSE]
  sRow = out[out$var == "s", , drop = FALSE]

  expect_match(fRow$details, "^Levels: ")
  expect_match(fRow$details, "\\.\\.\\.$")

  expect_match(sRow$details, "^Values: ")
  expect_match(sRow$details, "\\.\\.\\.$")
})

test_that("buildVarSummary errors on non-data-frame input", {
  expect_error(buildVarSummary(1), "data frame")
})
