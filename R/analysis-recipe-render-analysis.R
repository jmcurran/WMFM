#' Render post-fit analysis sections for a WMFM analysis recipe
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return Character vector containing Quarto source lines.
#'
#' @keywords internal
renderAnalysisRecipeAnalysisQuarto = function(recipe) {
  validateAnalysisRecipe(recipe)

  sectionLines = character(0)

  if (isTRUE(recipe$sections$summary$enabled %||% FALSE)) {
    sectionLines = c(
      sectionLines,
      "",
      "# Model summary",
      "",
      renderAnalysisRecipeSummaryChunk(recipe)
    )
  }

  if (isTRUE(recipe$sections$anova$enabled %||% FALSE)) {
    sectionLines = c(
      sectionLines,
      "",
      "# Analysis of variance",
      "",
      renderAnalysisRecipeAnovaChunk(recipe)
    )
  }

  if (isTRUE(recipe$sections$confidenceIntervals$enabled %||% FALSE)) {
    sectionLines = c(
      sectionLines,
      "",
      "# Confidence intervals",
      "",
      renderAnalysisRecipeConfidenceIntervalChunk(recipe)
    )
  }

  if (isTRUE(recipe$sections$diagnostics$enabled %||% FALSE)) {
    sectionLines = c(
      sectionLines,
      "",
      "# Diagnostic plots",
      "",
      renderAnalysisRecipeDiagnosticChunk(recipe)
    )
  }

  if (isTRUE(recipe$sections$modelPlot$enabled %||% FALSE)) {
    sectionLines = c(
      sectionLines,
      "",
      "# Model plot",
      "",
      renderAnalysisRecipeModelPlotChunk(recipe)
    )
  }

  sectionLines
}

#' Render model-summary code
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeSummaryChunk = function(recipe) {
  validateAnalysisRecipe(recipe)
  renderAnalysisRecipeCodeChunk("model-summary", "summary(modelFit)")
}

#' Render analysis-of-variance code
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeAnovaChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  modelType = recipe$model$modelType %||% ""
  codeLine = if (modelType %in% c("logistic", "poisson")) {
    'anova(modelFit, test = "Chisq")'
  } else {
    "anova(modelFit)"
  }

  renderAnalysisRecipeCodeChunk("analysis-of-variance", codeLine)
}

#' Render confidence-interval code
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeConfidenceIntervalChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  level = recipe$sections$confidenceIntervals$level %||% 0.95
  renderAnalysisRecipeCodeChunk(
    "confidence-intervals",
    c(
      paste0("confidenceLevel = ", format(level, scientific = FALSE, trim = TRUE)),
      "confidenceIntervals = modelConfidenceIntervals(",
      "  model = modelFit,",
      "  level = confidenceLevel",
      ")",
      "confidenceIntervals"
    )
  )
}

#' Render diagnostic-plot code
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeDiagnosticChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  modelType = recipe$model$modelType %||% ""
  codeLines = if (identical(modelType, "lm")) {
    c(
      "oldPar = par(mfrow = c(2, 2))",
      "plot(modelFit)",
      "par(oldPar)"
    )
  } else {
    c(
      "diagnosticData = data.frame(",
      "  fitted = fitted(modelFit),",
      "  residual = residuals(modelFit, type = \"deviance\")",
      ")",
      "plot(",
      "  diagnosticData$fitted,",
      "  diagnosticData$residual,",
      "  xlab = \"Fitted values\",",
      "  ylab = \"Deviance residuals\"",
      ")",
      "abline(h = 0, lty = 2)"
    )
  }

  renderAnalysisRecipeCodeChunk("diagnostic-plots", codeLines)
}

#' Render the substantive model-plot code
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeModelPlotChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  plotType = recipe$sections$modelPlot$plotType %||% "observedFitted"
  showSmoothTrend = isTRUE(recipe$sections$modelPlot$showSmoothTrend %||% TRUE)

  renderAnalysisRecipeCodeChunk(
    "model-plot",
    c(
      "modelPlot = plotModelPlot(",
      "  model = modelFit,",
      paste0("  plotType = ", encodeAnalysisRecipeString(plotType), ","),
      paste0("  showSmoothTrend = ", if (showSmoothTrend) "TRUE" else "FALSE"),
      ")",
      "modelPlot"
    )
  )
}
