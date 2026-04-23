testthat::test_that("completeConfidenceIntervalNewData uses mean anchoring for omitted numeric predictors", {

  d = data.frame(
    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0),
    Locn = factor(
      c(rep("SC", 9), rep("WA", 9)),
      levels = c("SC", "WA")
    ),
    Magnitude = c(
      5.25, 5.50, 5.75, 6.00, 6.25, 6.50, 6.75, 7.00, 7.25,
      5.25, 5.50, 5.75, 6.00, 6.25, 6.50, 6.75, 7.00, 7.25
    )
  )

  fit = glm(Freq ~ Locn * Magnitude, data = d, family = poisson())

  out = completeConfidenceIntervalNewData(
    model = fit,
    newData = data.frame(Locn = factor("WA", levels = c("SC", "WA"))),
    numericReference = "mean"
  )

  testthat::expect_identical(names(out), c("Locn", "Magnitude"))
  testthat::expect_equal(out$Magnitude[1], mean(d$Magnitude))
})

testthat::test_that("completeConfidenceIntervalNewData uses zero anchoring when requested", {

  d = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    g = factor(c("a", "a", "b", "b", "a", "b")),
    x = c(-2, -1, 0, 1, 2, 3)
  )

  fit = lm(y ~ g + x, data = d)

  out = completeConfidenceIntervalNewData(
    model = fit,
    newData = data.frame(g = factor("b", levels = levels(d$g))),
    numericReference = "zero"
  )

  testthat::expect_identical(names(out), c("g", "x"))
  testthat::expect_equal(out$x[1], 0)
})
