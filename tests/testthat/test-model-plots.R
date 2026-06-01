testthat::test_that("buildModelPlotData supports lm observed and residual plots", {
  model = stats::lm(mpg ~ wt, data = mtcars)

  observedData = buildModelPlotData(model, plotType = "observedFitted")
  residualData = buildModelPlotData(model, plotType = "residualFitted")

  testthat::expect_true(observedData$available)
  testthat::expect_identical(observedData$plotFamily, "lm")
  testthat::expect_identical(observedData$fittedScale, "response")
  testthat::expect_identical(observedData$responseName, "mpg")
  testthat::expect_identical(observedData$labels$y, "Observed mpg")
  testthat::expect_identical(residualData$residualType, "ordinary")
  testthat::expect_equal(nrow(observedData$data), stats::nobs(model))
  testthat::expect_named(
    observedData$data,
    c("observation", "observed", "fitted", "residual")
  )
})

testthat::test_that("buildModelPlotData supports logistic regression on probability scale", {
  data = mtcars
  data$amFactor = factor(data$am, levels = c(0, 1), labels = c("automatic", "manual"))
  model = stats::glm(amFactor ~ wt, data = data, family = stats::binomial())

  plotData = buildModelPlotData(model, plotType = "residualFitted")

  testthat::expect_true(plotData$available)
  testthat::expect_identical(plotData$plotFamily, "binomial")
  testthat::expect_identical(plotData$fittedScale, "predicted probability")
  testthat::expect_identical(plotData$residualType, "deviance")
  testthat::expect_identical(plotData$responseName, "amFactor")
  testthat::expect_true(all(plotData$data$fitted >= 0 & plotData$data$fitted <= 1))
  testthat::expect_identical(plotData$yLabels, c("automatic", "manual"))
})

testthat::test_that("buildModelPlotData supports poisson regression on fitted-count scale", {
  data = warpbreaks
  model = stats::glm(breaks ~ wool + tension, data = data, family = stats::poisson())

  plotData = buildModelPlotData(model, plotType = "observedFitted")

  testthat::expect_true(plotData$available)
  testthat::expect_identical(plotData$plotFamily, "poisson")
  testthat::expect_identical(plotData$fittedScale, "fitted count")
  testthat::expect_identical(plotData$residualType, "deviance")
  testthat::expect_identical(plotData$labels$x, "Fitted count")
  testthat::expect_identical(plotData$labels$y, "Observed breaks")
  testthat::expect_true(all(plotData$data$fitted > 0))
})

testthat::test_that("model plot choices are model-aware", {
  lmModel = stats::lm(mpg ~ wt, data = mtcars)
  binomialModel = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  poissonModel = stats::glm(breaks ~ wool, data = warpbreaks, family = stats::poisson())

  testthat::expect_named(
    buildModelPlotTypeChoices(lmModel),
    c("Observed vs fitted", "Residuals vs fitted")
  )
  testthat::expect_named(
    buildModelPlotTypeChoices(binomialModel),
    c(
      "Observed outcome vs predicted probability",
      "Deviance residuals vs fitted probability"
    )
  )
  testthat::expect_named(
    buildModelPlotTypeChoices(poissonModel),
    c("Observed count vs fitted count", "Deviance residuals vs fitted count")
  )
})

testthat::test_that("unsupported model plot choices do not make plotting helpers fail", {
  model = stats::glm(mpg ~ wt, data = mtcars, family = stats::Gamma())

  choices = buildModelPlotTypeChoices(model)
  plotData = buildModelPlotData(model, plotType = unname(choices)[1])
  note = buildModelPlotTeachingNote(model, plotType = unname(choices)[1])
  plot = plotModelPlot(model, plotType = unname(choices)[1])

  testthat::expect_identical(choices, c("No model plots available" = "unsupported"))
  testthat::expect_false(plotData$available)
  testthat::expect_identical(note$title, "Model plots are not available for this fitted model.")
  testthat::expect_null(plot)
})

testthat::test_that("model plot teaching notes use cautious language", {
  model = stats::lm(mpg ~ wt, data = mtcars)

  note = buildModelPlotTeachingNote(model, plotType = "observedFitted")
  noteText = paste(unlist(note), collapse = " ")

  testthat::expect_match(noteText, "missing structure", fixed = TRUE)
  testthat::expect_match(noteText, "does not prove", fixed = TRUE)
  testthat::expect_no_match(noteText, "assumptions are satisfied", fixed = TRUE)
  testthat::expect_no_match(noteText, "valid", fixed = TRUE)
  testthat::expect_no_match(noteText, "invalid", fixed = TRUE)
})

testthat::test_that("plotModelPlot uses model-aware labels", {
  model = stats::lm(mpg ~ wt, data = mtcars)

  plot = plotModelPlot(model, plotType = "observedFitted")

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_identical(plot$labels$x, "Fitted value")
  testthat::expect_identical(plot$labels$y, "Observed mpg")
  testthat::expect_identical(plot$labels$title, "Observed vs fitted")
})

testthat::test_that("model plots UI has approved label and avoids checking language", {
  uiText = paste(deparse(body(appUI)), collapse = "\n")

  testthat::expect_match(uiText, "Model plots", fixed = TRUE)
  testthat::expect_match(uiText, "missing obvious structure", fixed = TRUE)
  testthat::expect_no_match(uiText, "Model checking", fixed = TRUE)
  testthat::expect_no_match(uiText, "Diagnostic plots", fixed = TRUE)
  testthat::expect_no_match(uiText, "Assumption checks", fixed = TRUE)
  testthat::expect_no_match(uiText, "Model validity", fixed = TRUE)
})

testthat::test_that("model plot summary text reports plotted scale and observation count", {
  lmModel = stats::lm(mpg ~ wt, data = mtcars)
  binomialModel = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())

  observedSummary = buildModelPlotSummaryText(lmModel, plotType = "observedFitted")
  residualSummary = buildModelPlotSummaryText(binomialModel, plotType = "residualFitted")

  testthat::expect_match(observedSummary, "Plotting 32 observations", fixed = TRUE)
  testthat::expect_match(observedSummary, "response scale", fixed = TRUE)
  testthat::expect_match(residualSummary, "deviance residuals", fixed = TRUE)
  testthat::expect_match(residualSummary, "predicted probability", fixed = TRUE)
})

testthat::test_that("model plots UI includes deterministic plot summary output", {
  uiText = paste(deparse(body(appUI)), collapse = "\n")
  observerText = paste(deparse(body(registerModelPlotObservers)), collapse = "\n")

  testthat::expect_match(uiText, "modelPlotSummaryUi", fixed = TRUE)
  testthat::expect_match(observerText, "output$modelPlotSummaryUi", fixed = TRUE)
  testthat::expect_match(observerText, "buildModelPlotSummaryText", fixed = TRUE)
})
