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



testthat::test_that("plotModelPlot draws red reference and logistic trend layers", {
  lmModel = stats::lm(mpg ~ wt, data = mtcars)
  lmPlot = plotModelPlot(lmModel, plotType = "observedFitted")

  testthat::expect_equal(length(lmPlot$layers), 2)
  testthat::expect_identical(lmPlot$layers[[2]]$aes_params$colour, "red")
  testthat::expect_identical(lmPlot$layers[[2]]$aes_params$linewidth, 2)

  poissonModel = stats::glm(
    breaks ~ wool + tension,
    data = warpbreaks,
    family = stats::poisson()
  )
  poissonPlot = plotModelPlot(poissonModel, plotType = "residualFitted")

  testthat::expect_equal(length(poissonPlot$layers), 2)
  testthat::expect_identical(poissonPlot$layers[[2]]$aes_params$colour, "red")
  testthat::expect_identical(poissonPlot$layers[[2]]$aes_params$linewidth, 2)

  data = mtcars
  data$amFactor = factor(data$am, levels = c(0, 1), labels = c("automatic", "manual"))
  logisticModel = stats::glm(
    amFactor ~ wt,
    data = data,
    family = stats::binomial()
  )
  logisticPlot = plotModelPlot(logisticModel, plotType = "observedFitted")

  testthat::expect_equal(length(logisticPlot$layers), 2)
  testthat::expect_identical(logisticPlot$layers[[2]]$aes_params$colour, "red")
  testthat::expect_identical(logisticPlot$layers[[2]]$aes_params$linetype, "dashed")
})




testthat::test_that("model plots tab follows model explanation and precedes plot tab", {
  uiText = paste(deparse(body(appUI)), collapse = "\n")

  explanationPosition = regexpr('"Model Explanation"', uiText, fixed = TRUE)[1]
  modelPlotsPosition = regexpr('"Model plots"', uiText, fixed = TRUE)[1]
  plotPosition = regexpr('"Plot"', uiText, fixed = TRUE)[1]

  testthat::expect_gt(explanationPosition, 0)
  testthat::expect_gt(modelPlotsPosition, explanationPosition)
  testthat::expect_gt(plotPosition, modelPlotsPosition)
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


testthat::test_that("developer tabs appear after ordinary student tabs", {
  uiText = paste(deparse(body(appUI)), collapse = "\n")
  observerText = paste(deparse(body(registerDeveloperScoringGradingObservers)), collapse = "\n")

  plotPosition = regexpr('"Plot"', uiText, fixed = TRUE)[1]
  variableExplorerPosition = regexpr('"Variable Explorer"', uiText, fixed = TRUE)[1]
  settingsPosition = regexpr('"Settings"', uiText, fixed = TRUE)[1]
  scoringTargetPosition = regexpr('target = "Variable Explorer"', observerText, fixed = TRUE)[1]

  testthat::expect_gt(variableExplorerPosition, plotPosition)
  testthat::expect_gt(settingsPosition, variableExplorerPosition)
  testthat::expect_gt(scoringTargetPosition, 0)
})
