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
      renderAnalysisRecipeSummaryChunk(recipe),
      "",
      renderAnalysisRecipeFittedModelSection(recipe)
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

#' Render the fitted-model section appropriate to the model structure
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto heading, explanatory text,
#'   and code chunk.
#'
#' @keywords internal
#' @noRd
renderAnalysisRecipeFittedModelSection = function(recipe) {
  validateAnalysisRecipe(recipe)

  factorLevels = recipe$model$factorLevels %||% list()
  predictors = recipe$model$predictors %||% character(0)
  factorOnly = length(predictors) > 0 &&
    all(predictors %in% names(factorLevels))

  if (!factorOnly) {
    return(c(
      "# Fitted equation",
      "",
      "The following code constructs the fitted equation directly from the estimated regression coefficients.",
      "",
      renderAnalysisRecipeEquationChunk(recipe)
    ))
  }

  c(
    "# Fitted means",
    "",
    "The following code calculates the fitted mean for every combination of factor levels. The accompanying confidence intervals describe uncertainty in those fitted means rather than repeating confidence intervals for every regression coefficient.",
    "",
    renderAnalysisRecipeFittedMeansChunk(recipe)
  )
}

#' Render fitted means and their confidence intervals for factor-only models
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
#' @noRd
renderAnalysisRecipeFittedMeansChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  factorLevels = recipe$model$factorLevels %||% list()
  predictors = recipe$model$predictors %||% character(0)
  factorLevels = factorLevels[predictors]
  levelLines = vapply(
    seq_along(factorLevels),
    function(index) {
      suffix = if (index < length(factorLevels)) "," else ""
      values = paste(
        vapply(factorLevels[[index]], encodeAnalysisRecipeString, character(1)),
        collapse = ", "
      )
      paste0("  ", names(factorLevels)[index], " = c(", values, ")", suffix)
    },
    character(1)
  )

  codeLines = c(
    "factorLevels = list(",
    levelLines,
    ")",
    "",
    "fittedMeanData = expand.grid(",
    "  factorLevels,",
    "  KEEP.OUT.ATTRS = FALSE,",
    "  stringsAsFactors = FALSE",
    ")",
    "",
    "for (variable in names(factorLevels)) {",
    "  fittedMeanData[[variable]] = factor(",
    "    fittedMeanData[[variable]],",
    "    levels = factorLevels[[variable]]",
    "  )",
    "}",
    ""
  )

  modelType = recipe$model$modelType %||% "lm"
  if (identical(modelType, "lm")) {
    codeLines = c(
      codeLines,
      "fittedMeanIntervals = predict(",
      "  modelFit,",
      "  newdata = fittedMeanData,",
      "  interval = \"confidence\",",
      "  level = 0.95",
      ")",
      "",
      "fittedMeans = cbind(",
      "  fittedMeanData,",
      "  as.data.frame(fittedMeanIntervals)",
      ")",
      "fittedMeans"
    )
  } else {
    codeLines = c(
      codeLines,
      "linkPredictions = predict(",
      "  modelFit,",
      "  newdata = fittedMeanData,",
      "  type = \"link\",",
      "  se.fit = TRUE",
      ")",
      "criticalValue = qnorm(0.975)",
      "inverseLink = family(modelFit)$linkinv",
      "",
      "fittedMeans = data.frame(",
      "  fittedMeanData,",
      "  fit = inverseLink(linkPredictions$fit),",
      "  lower = inverseLink(linkPredictions$fit - criticalValue * linkPredictions$se.fit),",
      "  upper = inverseLink(linkPredictions$fit + criticalValue * linkPredictions$se.fit)",
      ")",
      "fittedMeans"
    )
  }

  renderAnalysisRecipeCodeChunk("fitted-means", codeLines)
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

#' Render explicit fitted-equation code
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
#' @noRd
renderAnalysisRecipeEquationChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  responseName = recipe$model$response %||% "response"
  modelType = recipe$model$modelType %||% "lm"
  equationLeft = if (identical(modelType, "logistic")) {
    paste0("logit(P(", responseName, " = 1))")
  } else if (identical(modelType, "poisson")) {
    paste0("log(E(", responseName, "))")
  } else {
    paste0("E(", responseName, ")")
  }

  renderAnalysisRecipeCodeChunk(
    "fitted-equation",
    c(
      "modelCoefficients = coef(modelFit)",
      "modelCoefficients",
      "",
      "formatEquationTerm = function(termName, estimate, firstTerm = FALSE) {",
      "  estimateText = format(round(abs(estimate), 4), trim = TRUE)",
      "  if (firstTerm) {",
      "    return(format(round(estimate, 4), trim = TRUE))",
      "  }",
      "  signText = if (estimate < 0) \" - \" else \" + \"",
      "  paste0(signText, estimateText, \" * \", termName)",
      "}",
      "",
      "equationTerms = vapply(",
      "  seq_along(modelCoefficients),",
      "  function(index) {",
      "    termName = names(modelCoefficients)[index]",
      "    if (identical(termName, \"(Intercept)\")) {",
      "      termName = \"\"",
      "    }",
      "    formatEquationTerm(",
      "      termName = termName,",
      "      estimate = modelCoefficients[index],",
      "      firstTerm = index == 1",
      "    )",
      "  },",
      "  character(1)",
      ")",
      paste0("equationText = paste0(", encodeAnalysisRecipeString(paste0(equationLeft, " = ")), ", paste(equationTerms, collapse = \"\"))"),
      "equationText"
    )
  )
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

#' Render explicit coefficient confidence-interval code
#'
#' @inheritParams renderAnalysisRecipeAnalysisQuarto
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeConfidenceIntervalChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  level = recipe$sections$confidenceIntervals$level %||% 0.95
  modelType = recipe$model$modelType %||% "lm"
  criticalValueLine = if (identical(modelType, "lm")) {
    "criticalValue = qt(1 - alpha / 2, df = df.residual(modelFit))"
  } else {
    "criticalValue = qnorm(1 - alpha / 2)"
  }

  codeLines = c(
    paste0("confidenceLevel = ", format(level, scientific = FALSE, trim = TRUE)),
    "alpha = 1 - confidenceLevel",
    "coefficientEstimate = coef(modelFit)",
    "coefficientCovariance = vcov(modelFit)",
    "coefficientStandardError = sqrt(diag(coefficientCovariance))",
    criticalValueLine,
    "",
    "coefficientIntervals = data.frame(",
    "  term = names(coefficientEstimate),",
    "  estimate = unname(coefficientEstimate),",
    "  standardError = unname(coefficientStandardError),",
    "  lower = unname(coefficientEstimate - criticalValue * coefficientStandardError),",
    "  upper = unname(coefficientEstimate + criticalValue * coefficientStandardError),",
    "  row.names = NULL",
    ")",
    "coefficientIntervals"
  )

  if (modelType %in% c("logistic", "poisson")) {
    effectLabel = if (identical(modelType, "logistic")) "oddsRatio" else "countMultiplier"
    codeLines = c(
      codeLines,
      "",
      "# Exponentiating converts coefficient effects from the link scale",
      "# to odds ratios or expected-count multipliers.",
      "multiplicativeIntervals = data.frame(",
      "  term = coefficientIntervals$term,",
      paste0("  ", effectLabel, " = exp(coefficientIntervals$estimate),"),
      "  lower = exp(coefficientIntervals$lower),",
      "  upper = exp(coefficientIntervals$upper),",
      "  row.names = NULL",
      ")",
      "multiplicativeIntervals"
    )
  }

  renderAnalysisRecipeCodeChunk("confidence-intervals", codeLines)
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

#' Render standalone ggplot2 code for the substantive model plot
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
  modelType = recipe$model$modelType %||% "lm"
  responseName = recipe$model$response %||% "Observed response"

  if (identical(plotType, "residualFitted")) {
    codeLines = c(
      "modelPlotData = data.frame(",
      "  fitted = fitted(modelFit),",
      "  residual = residuals(modelFit, type = \"response\")",
      ")",
      "",
      "modelPlot = ggplot(",
      "  modelPlotData,",
      "  aes(x = fitted, y = residual)",
      ") +",
      "  geom_point(alpha = 0.6) +",
      "  geom_hline(yintercept = 0, linetype = \"dashed\", colour = \"red\") +",
      "  labs(",
      "    title = \"Residuals versus fitted values\",",
      "    x = \"Fitted values\",",
      "    y = \"Residuals\"",
      "  ) +",
      "  theme_minimal()"
    )
    if (showSmoothTrend && identical(modelType, "lm")) {
      codeLines = c(
        codeLines,
        "",
        "modelPlot = modelPlot + geom_smooth(",
        "  method = \"loess\",",
        "  formula = y ~ x,",
        "  se = FALSE,",
        "  colour = \"blue\"",
        ")"
      )
    }
  } else {
    codeLines = c(
      "modelPlotData = data.frame(",
      "  observed = model.response(model.frame(modelFit)),",
      "  fitted = fitted(modelFit)",
      ")",
      ""
    )

    if (identical(modelType, "logistic")) {
      codeLines = c(
        codeLines,
        "modelPlot = ggplot(",
        "  modelPlotData,",
        "  aes(x = fitted, y = observed)",
        ") +",
        "  geom_jitter(height = 0.04, width = 0, alpha = 0.6) +",
        "  geom_smooth(",
        "    method = \"glm\",",
        "    method.args = list(family = binomial()),",
        "    formula = y ~ x,",
        "    se = FALSE,",
        "    linetype = \"dashed\",",
        "    colour = \"red\"",
        "  ) +",
        "  scale_y_continuous(breaks = c(0, 1)) +"
      )
    } else {
      codeLines = c(
        codeLines,
        "modelPlot = ggplot(",
        "  modelPlotData,",
        "  aes(x = fitted, y = observed)",
        ") +",
        "  geom_point(alpha = 0.6) +"
      )
      if (showSmoothTrend && identical(modelType, "lm")) {
        codeLines = c(
          codeLines,
          "  geom_smooth(",
          "    method = \"loess\",",
          "    formula = y ~ x,",
          "    se = FALSE,",
          "    colour = \"blue\"",
          "  ) +"
        )
      }
      codeLines = c(
        codeLines,
        "  geom_abline(",
        "    slope = 1,",
        "    intercept = 0,",
        "    linetype = \"dashed\",",
        "    colour = \"red\"",
        "  ) +"
      )
    }

    codeLines = c(
      codeLines,
      "  labs(",
      "    title = \"Observed versus fitted values\",",
      "    x = \"Fitted values\",",
      paste0("    y = ", encodeAnalysisRecipeString(responseName)),
      "  ) +",
      "  theme_minimal()"
    )
  }

  renderAnalysisRecipeCodeChunk(
    "model-plot",
    c(codeLines, "", "modelPlot")
  )
}
