test_that("factor-only plot controls offer boxplot and beeswarm styles", {
  ui = plotCiControlsUi(mode = "factorOnly")
  html = htmltools::renderTags(ui)$html

  expect_match(html, "factorPlotType", fixed = TRUE)
  expect_match(html, "Boxplot", fixed = TRUE)
  expect_match(html, "Beeswarm", fixed = TRUE)
})

test_that("factor-only beeswarm plots centre fitted means and intervals", {
  plotDeviceFile = tempfile(fileext = ".pdf")
  defaultPlotFile = file.path(getwd(), "Rplots.pdf")
  grDevices::pdf(plotDeviceFile)
  plotDevice = grDevices::dev.cur()

  on.exit({
    if (plotDevice %in% grDevices::dev.list()) {
      grDevices::dev.off(plotDevice)
    }
    unlink(c(plotDeviceFile, defaultPlotFile))
  }, add = TRUE)

  model = lm(Sepal.Length ~ Species, data = iris)

  plot = makeFactorOnlyPlot(
    model = model,
    data = model.frame(model),
    plotType = "beeswarm"
  )

  expect_s3_class(plot, "ggplot")
  beeswarmLayer = plot$layers[[1]]
  intervalLayer = plot$layers[[2]]

  expect_s3_class(beeswarmLayer$position, "PositionBeeswarm")
  expect_equal(beeswarmLayer$position$cex, 1.5)
  expect_equal(intervalLayer$geom_params$width, 0.07)

  built = ggplot2::ggplot_build(plot)
  fitPoint = built$data[[3]]
  fitInterval = built$data[[2]]

  expect_equal(fitPoint$x, seq_along(levels(iris$Species)))
  expect_equal(fitInterval$x, seq_along(levels(iris$Species)))
})

test_that("boxplots remain the default factor-only display", {
  model = lm(Sepal.Length ~ Species, data = iris)

  plot = makeFactorOnlyPlot(
    model = model,
    data = model.frame(model)
  )

  expect_true(any(vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomBoxplot")
  }, logical(1))))
})
