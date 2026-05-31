#' Add deterministic adjustment-comparison interpretation to a follow-up payload
#'
#' @param model Fitted model object.
#' @param followupPayload List returned by \code{classifyModelFollowupQuestion()}.
#'
#' @return Updated payload list. Adds \code{adjustmentComparisonResult} for
#'   supported adjustment-comparison requests.
#' @keywords internal
#' @noRd
enrichFollowupPayloadWithAdjustmentComparison = function(model, followupPayload) {
  payload = followupPayload
  if (!is.list(payload)) {
    return(payload)
  }

  if (!identical(payload$category, "adjustment_prediction_comparison")) {
    return(payload)
  }

  payload$adjustmentComparisonResult = computeModelQuestionAdjustmentComparison(
    model = model,
    followupQuestion = payload$originalText %||% ""
  )
  payload$requiresDeterministicComputation = TRUE
  payload
}

#' @keywords internal
#' @noRd
computeModelQuestionAdjustmentComparison = function(model, followupQuestion = "") {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_type",
      modelType = class(model)[[1]] %||% "unknown",
      comparisonType = "not_available",
      warnings = "Adjustment-comparison follow-ups currently require lm model objects."
    ))
  }

  modelFrame = stats::model.frame(model)
  logLog = getLogLogModelMetadata(model = model, modelFrame = modelFrame)
  if (!isTRUE(logLog$isLogLog)) {
    return(list(
      status = "unsupported",
      reason = "not_log_log_model",
      modelType = "lm",
      comparisonType = "not_available",
      warnings = "Adjustment-comparison follow-ups are currently deterministic only for log-log models."
    ))
  }

  termLabels = attr(stats::terms(model), "term.labels")
  logTermLabels = logLog$logPredictors$termLabel
  adjustmentTerms = setdiff(termLabels, logTermLabels)
  if (!length(adjustmentTerms)) {
    return(list(
      status = "unsupported",
      reason = "no_adjustment_terms",
      modelType = "lm",
      modelStructure = "log_log",
      comparisonType = "not_available",
      responseName = logLog$responseVariable,
      warnings = "The fitted log-log model does not contain adjustment terms to compare with the simpler model."
    ))
  }

  reducedFormula = makeLogLogReducedFormula(
    responseName = names(modelFrame)[[1]],
    logPredictorNames = logLog$logPredictors$transformedName
  )
  reducedModel = tryCatch(
    stats::lm(reducedFormula, data = modelFrame),
    error = function(e) {
      NULL
    }
  )

  if (is.null(reducedModel)) {
    return(list(
      status = "unsupported",
      reason = "reduced_model_failed",
      modelType = "lm",
      modelStructure = "log_log",
      comparisonType = "not_available",
      responseName = logLog$responseVariable,
      adjustmentTerms = adjustmentTerms,
      warnings = "WMFM could not fit the simpler log-log comparison model from the fitted model frame."
    ))
  }

  fullSummary = summary(model)
  reducedSummary = summary(reducedModel)
  fullR2 = unname(fullSummary$r.squared)
  reducedR2 = unname(reducedSummary$r.squared)
  fullAdjustedR2 = unname(fullSummary$adj.r.squared)
  reducedAdjustedR2 = unname(reducedSummary$adj.r.squared)
  fullSigma = unname(fullSummary$sigma)
  reducedSigma = unname(reducedSummary$sigma)
  fullAic = stats::AIC(model)
  reducedAic = stats::AIC(reducedModel)
  aicChange = fullAic - reducedAic
  adjustedR2Change = fullAdjustedR2 - reducedAdjustedR2
  sigmaPercentChange = 100 * (fullSigma / reducedSigma - 1)
  predictionImprovement = classifyAdjustmentPredictionImprovement(
    adjustedR2Change = adjustedR2Change,
    sigmaPercentChange = sigmaPercentChange,
    aicChange = aicChange
  )

  list(
    status = "ok",
    modelType = "lm",
    modelStructure = "log_log",
    comparisonType = "adjusted_vs_weight_only_log_log",
    responseName = logLog$responseVariable,
    responseScale = "log_response",
    primaryPredictors = logLog$logPredictors$originalName,
    adjustmentTerms = adjustmentTerms,
    fullModelTerms = termLabels,
    reducedModelTerms = logTermLabels,
    fullR2 = fullR2,
    reducedR2 = reducedR2,
    r2Change = fullR2 - reducedR2,
    fullAdjustedR2 = fullAdjustedR2,
    reducedAdjustedR2 = reducedAdjustedR2,
    adjustedR2Change = adjustedR2Change,
    fullSigma = fullSigma,
    reducedSigma = reducedSigma,
    sigmaChange = fullSigma - reducedSigma,
    sigmaPercentChange = sigmaPercentChange,
    fullAic = fullAic,
    reducedAic = reducedAic,
    aicChange = aicChange,
    predictionImprovement = predictionImprovement,
    interpretation = buildAdjustmentComparisonInterpretation(
      responseName = logLog$responseVariable,
      primaryPredictors = logLog$logPredictors$originalName,
      adjustmentTerms = adjustmentTerms,
      reducedAdjustedR2 = reducedAdjustedR2,
      fullAdjustedR2 = fullAdjustedR2,
      adjustedR2Change = adjustedR2Change,
      reducedSigma = reducedSigma,
      fullSigma = fullSigma,
      sigmaPercentChange = sigmaPercentChange,
      predictionImprovement = predictionImprovement
    ),
    directAnswer = buildAdjustmentComparisonDirectAnswer(
      adjustmentTerms = adjustmentTerms,
      reducedAdjustedR2 = reducedAdjustedR2,
      fullAdjustedR2 = fullAdjustedR2,
      adjustedR2Change = adjustedR2Change,
      reducedSigma = reducedSigma,
      fullSigma = fullSigma,
      sigmaPercentChange = sigmaPercentChange,
      predictionImprovement = predictionImprovement
    )
  )
}

#' @keywords internal
#' @noRd
makeLogLogReducedFormula = function(responseName, logPredictorNames) {
  stats::as.formula(paste(
    quoteModelFrameName(responseName),
    "~",
    paste(vapply(logPredictorNames, quoteModelFrameName, character(1)), collapse = " + ")
  ))
}

#' @keywords internal
#' @noRd
quoteModelFrameName = function(x) {
  paste0("`", gsub("`", "", as.character(x)), "`")
}

#' @keywords internal
#' @noRd
buildAdjustmentComparisonInterpretation = function(
    responseName,
    primaryPredictors,
    adjustmentTerms,
    reducedAdjustedR2,
    fullAdjustedR2,
    adjustedR2Change,
    reducedSigma,
    fullSigma,
    sigmaPercentChange,
    predictionImprovement) {
  predictorText = paste(primaryPredictors, collapse = ", ")
  adjustmentText = paste(adjustmentTerms, collapse = ", ")
  sigmaDirection = formatAdjustmentSigmaDirection(sigmaPercentChange = sigmaPercentChange)
  responseScaleText = formatAdjustmentComparisonResponseScale(responseName = responseName)

  paste0(
    "Compared with the simpler log-log model using ", predictorText,
    ", adding ", adjustmentText,
    " changes the adjusted R-squared from ", signif(reducedAdjustedR2, 5),
    " to ", signif(fullAdjustedR2, 5),
    " and changes the residual standard error on the ", responseScaleText,
    " scale from ", signif(reducedSigma, 5),
    " to ", signif(fullSigma, 5),
    ". The adjusted R-squared change is ", signif(adjustedR2Change, 5),
    ", and the residual standard error is ", sigmaDirection,
    ". WMFM classifies this as ", predictionImprovement$label,
    " based on deterministic in-sample fit summaries."
  )
}

#' @keywords internal
#' @noRd
buildAdjustmentComparisonDirectAnswer = function(
    adjustmentTerms,
    reducedAdjustedR2,
    fullAdjustedR2,
    adjustedR2Change,
    reducedSigma,
    fullSigma,
    sigmaPercentChange,
    predictionImprovement) {
  adjustmentText = paste(adjustmentTerms, collapse = ", ")
  sigmaDirection = formatAdjustmentSigmaDirection(sigmaPercentChange = sigmaPercentChange)
  conclusion = switch(
    predictionImprovement$category %||% "not_available",
    substantial_in_sample_improvement = "yes, the adjusted model fits these data substantially better",
    modest_in_sample_improvement = "yes, the adjusted model fits these data somewhat better",
    little_in_sample_improvement = "not really; the adjusted model shows little in-sample improvement",
    "WMFM cannot make a clear deterministic judgement about the improvement"
  )

  paste0(
    "Direct answer: ", conclusion,
    " after adding ", adjustmentText,
    ". Adjusted R-squared changes from ", signif(reducedAdjustedR2, 4),
    " to ", signif(fullAdjustedR2, 4),
    " (change ", signif(adjustedR2Change, 4),
    "), and the residual standard error is ", sigmaDirection,
    " (", signif(reducedSigma, 4), " to ", signif(fullSigma, 4),
    "). This is an in-sample fit comparison, not evidence from a separate test set."
  )
}

#' @keywords internal
#' @noRd
formatAdjustmentSigmaDirection = function(sigmaPercentChange) {
  if (is.finite(sigmaPercentChange) && sigmaPercentChange < 0) {
    paste0("about ", signif(abs(sigmaPercentChange), 4), "% lower")
  } else if (is.finite(sigmaPercentChange) && sigmaPercentChange > 0) {
    paste0("about ", signif(abs(sigmaPercentChange), 4), "% higher")
  } else {
    "about the same"
  }
}

#' @keywords internal
#' @noRd
formatAdjustmentComparisonResponseScale = function(responseName) {
  responseName = as.character(responseName %||% "response")
  if (grepl("^log\\(", responseName)) {
    return(responseName)
  }

  paste0("log(", responseName, ")")
}

#' @keywords internal
#' @noRd
classifyAdjustmentPredictionImprovement = function(adjustedR2Change, sigmaPercentChange, aicChange) {
  if (!is.finite(adjustedR2Change) || !is.finite(sigmaPercentChange)) {
    return(list(
      category = "not_available",
      label = "not enough deterministic evidence to classify the improvement",
      rule = "requires finite adjusted R-squared and residual standard error changes"
    ))
  }

  if (adjustedR2Change >= 0.05 || sigmaPercentChange <= -10) {
    return(list(
      category = "substantial_in_sample_improvement",
      label = "a substantial in-sample improvement",
      rule = "adjusted R-squared increased by at least 0.05 or residual standard error fell by at least 10%"
    ))
  }

  if (adjustedR2Change >= 0.01 || sigmaPercentChange <= -5 || (is.finite(aicChange) && aicChange <= -10)) {
    return(list(
      category = "modest_in_sample_improvement",
      label = "a modest in-sample improvement",
      rule = "adjusted R-squared increased by at least 0.01, residual standard error fell by at least 5%, or AIC fell by at least 10"
    ))
  }

  list(
    category = "little_in_sample_improvement",
    label = "little in-sample improvement",
    rule = "adjusted R-squared, residual standard error, and AIC changes did not meet WMFM's modest-improvement thresholds"
  )
}
