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
    adjustedR2Change = fullAdjustedR2 - reducedAdjustedR2,
    fullSigma = fullSigma,
    reducedSigma = reducedSigma,
    sigmaChange = fullSigma - reducedSigma,
    sigmaPercentChange = 100 * (fullSigma / reducedSigma - 1),
    fullAic = fullAic,
    reducedAic = reducedAic,
    aicChange = fullAic - reducedAic,
    interpretation = buildAdjustmentComparisonInterpretation(
      responseName = logLog$responseVariable,
      primaryPredictors = logLog$logPredictors$originalName,
      adjustmentTerms = adjustmentTerms,
      reducedAdjustedR2 = reducedAdjustedR2,
      fullAdjustedR2 = fullAdjustedR2,
      adjustedR2Change = fullAdjustedR2 - reducedAdjustedR2,
      reducedSigma = reducedSigma,
      fullSigma = fullSigma,
      sigmaPercentChange = 100 * (fullSigma / reducedSigma - 1)
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
    sigmaPercentChange) {
  predictorText = paste(primaryPredictors, collapse = ", ")
  adjustmentText = paste(adjustmentTerms, collapse = ", ")
  sigmaDirection = if (is.finite(sigmaPercentChange) && sigmaPercentChange < 0) {
    paste0("about ", signif(abs(sigmaPercentChange), 4), "% lower")
  } else if (is.finite(sigmaPercentChange) && sigmaPercentChange > 0) {
    paste0("about ", signif(abs(sigmaPercentChange), 4), "% higher")
  } else {
    "about the same"
  }

  paste0(
    "Compared with the simpler log-log model using ", predictorText,
    ", adding ", adjustmentText,
    " changes the adjusted R-squared from ", signif(reducedAdjustedR2, 5),
    " to ", signif(fullAdjustedR2, 5),
    " and changes the residual standard error on the log(", responseName,
    ") scale from ", signif(reducedSigma, 5),
    " to ", signif(fullSigma, 5),
    ". The adjusted R-squared change is ", signif(adjustedR2Change, 5),
    ", and the residual standard error is ", sigmaDirection,
    "."
  )
}
