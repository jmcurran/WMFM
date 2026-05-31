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
  if (!inherits(model, "lm")) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_type",
      modelType = class(model)[[1]] %||% "unknown",
      comparisonType = "not_available",
      warnings = "Adjustment-comparison follow-ups currently require lm or glm model objects."
    ))
  }

  if (inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "glm_log_log_not_yet_supported",
      modelType = "glm",
      comparisonType = "not_available",
      warnings = "Adjustment-comparison follow-ups for GLMs should use likelihood and deviance diagnostics, but log-log GLM adjustment comparison is not yet implemented for this pathway."
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

  comparisonStats = computeNestedModelComparisonStats(
    reducedModel = reducedModel,
    fullModel = model
  )
  predictionImprovement = classifyAdjustmentPredictionImprovement(
    likelihoodRatioPValue = comparisonStats$likelihoodRatioPValue,
    aicChange = comparisonStats$aicChange,
    deviancePercentChange = comparisonStats$deviancePercentChange
  )

  list(
    status = "ok",
    modelType = "lm",
    modelStructure = "log_log",
    comparisonType = "adjusted_vs_weight_only_log_log",
    comparisonBasis = "nested_log_likelihood_comparison",
    responseName = logLog$responseVariable,
    responseScale = "log_response",
    primaryPredictors = logLog$logPredictors$originalName,
    adjustmentTerms = adjustmentTerms,
    fullModelTerms = termLabels,
    reducedModelTerms = logTermLabels,
    reducedLogLik = comparisonStats$reducedLogLik,
    fullLogLik = comparisonStats$fullLogLik,
    logLikChange = comparisonStats$logLikChange,
    likelihoodRatioStatistic = comparisonStats$likelihoodRatioStatistic,
    dfDifference = comparisonStats$dfDifference,
    likelihoodRatioPValue = comparisonStats$likelihoodRatioPValue,
    fullAic = comparisonStats$fullAic,
    reducedAic = comparisonStats$reducedAic,
    aicChange = comparisonStats$aicChange,
    fullDeviance = comparisonStats$fullDeviance,
    reducedDeviance = comparisonStats$reducedDeviance,
    devianceChange = comparisonStats$devianceChange,
    deviancePercentChange = comparisonStats$deviancePercentChange,
    fullSigma = comparisonStats$fullSigma,
    reducedSigma = comparisonStats$reducedSigma,
    sigmaChange = comparisonStats$sigmaChange,
    sigmaPercentChange = comparisonStats$sigmaPercentChange,
    predictionImprovement = predictionImprovement,
    studentFacingConclusion = buildAdjustmentComparisonStudentConclusion(
      adjustmentTerms = adjustmentTerms,
      predictionImprovement = predictionImprovement
    ),
    studentFacingCaution = "This is an in-sample comparison of the fitted models, not evidence from a separate test set or cross-validation.",
    interpretation = buildAdjustmentComparisonInterpretation(
      primaryPredictors = logLog$logPredictors$originalName,
      adjustmentTerms = adjustmentTerms,
      comparisonStats = comparisonStats,
      predictionImprovement = predictionImprovement
    ),
    directAnswer = buildAdjustmentComparisonDirectAnswer(
      adjustmentTerms = adjustmentTerms,
      comparisonStats = comparisonStats,
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
computeNestedModelComparisonStats = function(reducedModel, fullModel) {
  reducedLogLikObj = stats::logLik(reducedModel)
  fullLogLikObj = stats::logLik(fullModel)
  reducedLogLik = as.numeric(reducedLogLikObj)
  fullLogLik = as.numeric(fullLogLikObj)
  reducedDf = attr(reducedLogLikObj, "df")
  fullDf = attr(fullLogLikObj, "df")
  dfDifference = fullDf - reducedDf
  likelihoodRatioStatistic = 2 * (fullLogLik - reducedLogLik)
  likelihoodRatioPValue = if (is.finite(likelihoodRatioStatistic) && is.finite(dfDifference) && dfDifference > 0) {
    stats::pchisq(likelihoodRatioStatistic, df = dfDifference, lower.tail = FALSE)
  } else {
    NA_real_
  }

  fullAic = stats::AIC(fullModel)
  reducedAic = stats::AIC(reducedModel)
  fullDeviance = stats::deviance(fullModel)
  reducedDeviance = stats::deviance(reducedModel)
  fullSigma = extractModelSigma(fullModel)
  reducedSigma = extractModelSigma(reducedModel)

  list(
    reducedLogLik = reducedLogLik,
    fullLogLik = fullLogLik,
    logLikChange = fullLogLik - reducedLogLik,
    reducedDf = reducedDf,
    fullDf = fullDf,
    dfDifference = dfDifference,
    likelihoodRatioStatistic = likelihoodRatioStatistic,
    likelihoodRatioPValue = likelihoodRatioPValue,
    reducedAic = reducedAic,
    fullAic = fullAic,
    aicChange = fullAic - reducedAic,
    reducedDeviance = reducedDeviance,
    fullDeviance = fullDeviance,
    devianceChange = fullDeviance - reducedDeviance,
    deviancePercentChange = safePercentChange(fullDeviance, reducedDeviance),
    reducedSigma = reducedSigma,
    fullSigma = fullSigma,
    sigmaChange = fullSigma - reducedSigma,
    sigmaPercentChange = safePercentChange(fullSigma, reducedSigma)
  )
}

#' @keywords internal
#' @noRd
extractModelSigma = function(model) {
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    return(unname(summary(model)$sigma))
  }

  NA_real_
}

#' @keywords internal
#' @noRd
safePercentChange = function(newValue, oldValue) {
  if (!is.finite(newValue) || !is.finite(oldValue) || isTRUE(all.equal(oldValue, 0))) {
    return(NA_real_)
  }

  100 * (newValue / oldValue - 1)
}

#' @keywords internal
#' @noRd
buildAdjustmentComparisonInterpretation = function(
    primaryPredictors,
    adjustmentTerms,
    comparisonStats,
    predictionImprovement) {
  predictorText = paste(primaryPredictors, collapse = ", ")
  adjustmentText = paste(adjustmentTerms, collapse = ", ")
  devianceDirection = formatAdjustmentPercentDirection(
    percentChange = comparisonStats$deviancePercentChange,
    quantityName = "deviance"
  )
  aicDirection = formatAdjustmentDifferenceDirection(
    difference = comparisonStats$aicChange,
    quantityName = "AIC"
  )

  paste0(
    "Compared with the simpler log-log model using ", predictorText,
    ", adding ", adjustmentText,
    " improves the nested-model log-likelihood by ", signif(comparisonStats$logLikChange, 5),
    ". The likelihood-ratio statistic is ", signif(comparisonStats$likelihoodRatioStatistic, 5),
    " on ", comparisonStats$dfDifference,
    " degrees of freedom, with p-value ", signif(comparisonStats$likelihoodRatioPValue, 5),
    ". The ", devianceDirection,
    ", and the ", aicDirection,
    ". WMFM classifies this as ", predictionImprovement$label,
    " based on deterministic nested-model fit summaries. These diagnostics support the judgement but should not be named in the student-facing explanation unless the user explicitly asks."
  )
}

#' @keywords internal
#' @noRd
buildAdjustmentComparisonDirectAnswer = function(
    adjustmentTerms,
    comparisonStats,
    predictionImprovement) {
  adjustmentText = paste(adjustmentTerms, collapse = ", ")
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
    ". This judgement is based on a nested-model log-likelihood comparison",
    " supported by AIC and deviance summaries. This is an in-sample fit comparison, not evidence from a separate test set."
  )
}

#' @keywords internal
#' @noRd
buildAdjustmentComparisonStudentConclusion = function(adjustmentTerms, predictionImprovement) {
  adjustmentText = paste(adjustmentTerms, collapse = ", ")
  conclusion = switch(
    predictionImprovement$category %||% "not_available",
    substantial_in_sample_improvement = "Yes. For these data, accounting for the adjustment variables substantially improves the in-sample predictions compared with using weight alone.",
    modest_in_sample_improvement = "Yes, but the improvement is modest. For these data, accounting for the adjustment variables improves the in-sample predictions compared with using weight alone.",
    little_in_sample_improvement = "Not substantially. For these data, accounting for the adjustment variables adds little in-sample predictive improvement compared with using weight alone.",
    "WMFM cannot make a clear deterministic judgement about whether the adjustment variables improve prediction."
  )

  gsub("the adjustment variables", adjustmentText, conclusion, fixed = TRUE)
}

#' @keywords internal
#' @noRd
formatAdjustmentPercentDirection = function(percentChange, quantityName) {
  if (is.finite(percentChange) && percentChange < 0) {
    paste0(quantityName, " is about ", signif(abs(percentChange), 4), "% lower")
  } else if (is.finite(percentChange) && percentChange > 0) {
    paste0(quantityName, " is about ", signif(abs(percentChange), 4), "% higher")
  } else {
    paste0(quantityName, " is about the same")
  }
}

#' @keywords internal
#' @noRd
formatAdjustmentDifferenceDirection = function(difference, quantityName) {
  if (is.finite(difference) && difference < 0) {
    paste0(quantityName, " is lower by ", signif(abs(difference), 5))
  } else if (is.finite(difference) && difference > 0) {
    paste0(quantityName, " is higher by ", signif(abs(difference), 5))
  } else {
    paste0(quantityName, " is about the same")
  }
}

#' @keywords internal
#' @noRd
classifyAdjustmentPredictionImprovement = function(
    likelihoodRatioPValue,
    aicChange,
    deviancePercentChange) {
  hasLikelihoodSupport = is.finite(likelihoodRatioPValue)
  hasAicSupport = is.finite(aicChange)
  hasDevianceSupport = is.finite(deviancePercentChange)

  if (!hasLikelihoodSupport && !hasAicSupport && !hasDevianceSupport) {
    return(list(
      category = "not_available",
      label = "not enough deterministic evidence to classify the improvement",
      rule = "requires likelihood, AIC, or deviance comparison summaries"
    ))
  }

  if ((hasLikelihoodSupport && likelihoodRatioPValue < 0.001 && hasAicSupport && aicChange <= -10) ||
      (hasAicSupport && aicChange <= -20 && hasDevianceSupport && deviancePercentChange <= -10)) {
    return(list(
      category = "substantial_in_sample_improvement",
      label = "a substantial in-sample improvement",
      rule = "nested log-likelihood comparison is very strong with AIC support, or AIC and deviance both improve substantially"
    ))
  }

  if ((hasLikelihoodSupport && likelihoodRatioPValue < 0.05) ||
      (hasAicSupport && aicChange <= -2) ||
      (hasDevianceSupport && deviancePercentChange <= -5)) {
    return(list(
      category = "modest_in_sample_improvement",
      label = "a modest in-sample improvement",
      rule = "nested log-likelihood comparison, AIC, or deviance summaries show a modest improvement"
    ))
  }

  list(
    category = "little_in_sample_improvement",
    label = "little in-sample improvement",
    rule = "likelihood, AIC, and deviance summaries did not meet WMFM's modest-improvement thresholds"
  )
}
