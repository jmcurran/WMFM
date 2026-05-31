testthat::test_that("drawModelPlot supports direct log-log formulas", {
  dat = data.frame(
    price = c(1000, 1500, 2200, 3100, 4600, 6900),
    carat = c(0.3, 0.4, 0.55, 0.75, 1.0, 1.4)
  )

  model = lm(log(price) ~ log(carat), data = dat)

  plotObj = drawModelPlot(model)

  testthat::expect_s3_class(plotObj, "ggplot")
})

testthat::test_that("drawModelPlot supports direct log-log formulas with confidence bands", {
  dat = data.frame(
    price = c(1000, 1500, 2200, 3100, 4600, 6900),
    carat = c(0.3, 0.4, 0.55, 0.75, 1.0, 1.4)
  )

  model = lm(log(price) ~ log(carat), data = dat)

  plotObj = drawModelPlot(model, showCi = TRUE)

  testthat::expect_s3_class(plotObj, "ggplot")
})
