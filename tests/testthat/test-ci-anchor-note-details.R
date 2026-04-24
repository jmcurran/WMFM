testthat::test_that("buildDerivedModeNote includes detailed mean anchor values", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5.25, 5.50, 5.75, 6.00),
    g = factor(c("a", "a", "b", "b"))
  )

  out = buildDerivedModeNote(mf = d, numericReference = "mean")

  testthat::expect_match(out, "Factor predictors use their base levels by default: g = a.", fixed = TRUE)
  testthat::expect_match(out, "x = mean(x) = 5.625", fixed = TRUE)
  testthat::expect_match(out, "observed range [5.25, 6]", fixed = TRUE)
})

testthat::test_that("buildDerivedModeNote includes detailed zero anchor values", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(-1, 0, 1, 2)
  )

  out = buildDerivedModeNote(mf = d, numericReference = "zero")

  testthat::expect_match(out, "Numeric predictors are fixed at 0 for fitted-quantity rows", fixed = TRUE)
  testthat::expect_match(out, "x = 0 (observed range [-1, 2])", fixed = TRUE)
})

testthat::test_that("buildOtherBaseSettingsText includes detailed mean anchor values", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(10, 12, 14, 16),
    z = c(3, 4, 5, 6),
    g = factor(c("a", "a", "b", "b"))
  )

  baseRow = data.frame(
    x = mean(d$x),
    z = mean(d$z),
    g = factor("a", levels = levels(d$g))
  )

  out = buildOtherBaseSettingsText(
    mf = d,
    baseRow = baseRow,
    excludeVarName = "x",
    numericReference = "mean"
  )

  testthat::expect_match(out, "Other factors fixed at base levels: g = a", fixed = TRUE)
  testthat::expect_match(out, "z = mean(z) = 4.5", fixed = TRUE)
  testthat::expect_match(out, "observed range [3, 6]", fixed = TRUE)
})

testthat::test_that("buildOtherBaseSettingsText includes detailed zero anchor values", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(-2, -1, 0, 1),
    z = c(-1, 0, 1, 2)
  )

  baseRow = data.frame(x = 0, z = 0)

  out = buildOtherBaseSettingsText(
    mf = d,
    baseRow = baseRow,
    excludeVarName = "x",
    numericReference = "zero"
  )

  testthat::expect_match(out, "z = 0 (observed range [-1, 2])", fixed = TRUE)
})
