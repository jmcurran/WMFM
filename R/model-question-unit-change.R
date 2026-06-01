#' Add deterministic unit-change interpretation to a follow-up payload
#'
#' @param model Fitted model object.
#' @param followupPayload List returned by \\code{classifyModelFollowupQuestion()}.
#'
#' @return Updated payload list. Adds \\code{unitChangeResult} for supported
#'   unit-change requests.
#' @keywords internal
#' @noRd
enrichFollowupPayloadWithUnitChange = function(model, followupPayload) {
  payload = followupPayload
  if (!is.list(payload)) {
    return(payload)
  }

  if (identical(payload$category, "unit_change_request")) {
    unitChange = payload$unitChangeValues %||% numeric(0)
    payload$unitChangeResult = computeModelQuestionUnitChange(
      model = model,
      followupQuestion = payload$originalText %||% "",
      requestedUnitChange = unitChange
    )
    payload$requiresDeterministicComputation = TRUE
    return(payload)
  }

  if (identical(payload$category, "proportional_change_request")) {
    proportionalChange = payload$proportionalChangeValues %||% numeric(0)
    payload$unitChangeResult = computeModelQuestionProportionalChange(
      model = model,
      followupQuestion = payload$originalText %||% "",
      requestedPercentChange = proportionalChange
    )
    payload$requiresDeterministicComputation = TRUE
    return(payload)
  }

  payload
}

#' @keywords internal
#' @noRd
computeModelQuestionUnitChange = function(model, followupQuestion, requestedUnitChange = numeric(0)) {
  if (!inherits(model, "lm")) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_type",
      modelType = class(model)[[1]] %||% "unknown",
      effectScale = "not_available",
      warnings = "Unit-change follow-ups currently require lm or glm model objects."
    ))
  }

  unitChange = resolveRequestedUnitChange(requestedUnitChange = requestedUnitChange)
  modelType = if (inherits(model, "glm")) {
    "glm"
  } else {
    "lm"
  }
  defaultScale = if (identical(modelType, "glm")) {
    "not_available"
  } else {
    "response_difference"
  }

  if (!is.finite(unitChange) || unitChange <= 0) {
    return(list(
      status = "needs_input",
      reason = "missing_or_invalid_unit_change",
      modelType = modelType,
      effectScale = defaultScale,
      warnings = "Provide one positive numeric unit-change size, such as a 0.1-unit or 5-unit increase."
    ))
  }

  mf = stats::model.frame(model)
  responseName = names(mf)[[1]]
  predictorNames = names(mf)[-1]
  numericPredictors = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (!length(numericPredictors)) {
    return(list(
      status = "unsupported",
      reason = "no_numeric_predictors",
      modelType = modelType,
      effectScale = defaultScale,
      warnings = "The fitted model does not contain a numeric predictor that can be re-expressed as a unit-change effect."
    ))
  }

  predictorResolution = resolveUnitChangePredictor(
    followupQuestion = followupQuestion,
    numericPredictors = numericPredictors
  )
  if (!isTRUE(predictorResolution$ok)) {
    return(c(
      list(
        status = predictorResolution$status,
        reason = predictorResolution$reason,
        modelType = modelType,
        effectScale = defaultScale,
        requestedUnitChange = unitChange,
        candidatePredictors = numericPredictors
      ),
      predictorResolution[c("warnings")]
    ))
  }

  predictorName = predictorResolution$predictorName
  logLog = getLogLogModelMetadata(model = model, modelFrame = mf)
  if (isTRUE(logLog$isLogLog)) {
    logLogPredictor = matchLogLogUnitChangePredictor(
      logLog = logLog,
      predictorName = predictorName
    )

    if (!is.null(logLogPredictor)) {
      return(computeLogLogUnitChangeResult(
        model = model,
        modelFrame = mf,
        responseName = responseName,
        transformedPredictorName = logLogPredictor$transformedName,
        originalPredictorName = logLogPredictor$originalName,
        unitChange = unitChange
      ))
    }
  }

  coefValues = stats::coef(model)
  if (!(predictorName %in% names(coefValues))) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_structure",
      modelType = modelType,
      effectScale = defaultScale,
      requestedUnitChange = unitChange,
      predictorName = predictorName,
      warnings = "The requested predictor does not have a simple one-coefficient effect in the fitted model. Interaction, polynomial, transformed, or contrast-coded terms require a later deterministic pathway."
    ))
  }

  oneUnitEffect = unname(as.numeric(coefValues[[predictorName]]))
  ciResult = tryCatch(
    stats::confint(model, parm = predictorName),
    error = function(e) {
      NULL
    }
  )

  if (inherits(model, "glm")) {
    return(computeGlmUnitChangeResult(
      model = model,
      responseName = responseName,
      predictorName = predictorName,
      unitChange = unitChange,
      oneUnitEffect = oneUnitEffect,
      ciResult = ciResult
    ))
  }

  confidenceInterval = NULL
  ciValues = extractUnitChangeConfidenceLimits(ciResult)
  if (length(ciValues) == 2L) {
    confidenceInterval = list(
      level = 0.95,
      oneUnitLwr = unname(ciValues[[1]]),
      oneUnitUpr = unname(ciValues[[2]]),
      lwr = unname(ciValues[[1]]) * unitChange,
      upr = unname(ciValues[[2]]) * unitChange
    )
  }

  transformedEstimate = oneUnitEffect * unitChange
  list(
    status = "ok",
    modelType = "lm",
    effectScale = "response_difference",
    responseName = responseName,
    predictorName = predictorName,
    requestedUnitChange = unitChange,
    oneUnitEffect = oneUnitEffect,
    transformedEstimate = transformedEstimate,
    unitChangeEffect = transformedEstimate,
    confidenceInterval = confidenceInterval,
    interpretation = paste0(
      "For a ", signif(unitChange, 6), "-unit increase in ", predictorName,
      ", the fitted mean ", responseName, " changes by ",
      signif(transformedEstimate, 6), "."
    )
  )
}


#' @keywords internal
#' @noRd
computeModelQuestionProportionalChange = function(model, followupQuestion, requestedPercentChange = numeric(0)) {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_type",
      modelType = class(model)[[1]] %||% "unknown",
      effectScale = "not_available",
      warnings = "Proportional-change follow-ups currently require lm log-log model objects."
    ))
  }

  percentChange = resolveRequestedUnitChange(requestedUnitChange = requestedPercentChange)
  if (!is.finite(percentChange) || percentChange <= 0) {
    return(list(
      status = "needs_input",
      reason = "missing_or_invalid_proportional_change",
      modelType = "lm",
      effectScale = "not_available",
      warnings = "Provide one positive percentage change, such as a 10% increase or doubling."
    ))
  }

  mf = stats::model.frame(model)
  responseName = names(mf)[[1]]
  logLog = getLogLogModelMetadata(model = model, modelFrame = mf)
  if (!isTRUE(logLog$isLogLog)) {
    return(list(
      status = "unsupported",
      reason = "not_log_log_model",
      modelType = "lm",
      effectScale = "not_available",
      responseName = responseName,
      requestedPercentChange = percentChange,
      warnings = "Percentage-change follow-ups are currently deterministic only for log-log models."
    ))
  }

  predictorNames = logLog$logPredictors$originalName
  predictorResolution = resolveUnitChangePredictor(
    followupQuestion = followupQuestion,
    numericPredictors = predictorNames
  )
  if (!isTRUE(predictorResolution$ok)) {
    return(c(
      list(
        status = predictorResolution$status,
        reason = predictorResolution$reason,
        modelType = "lm",
        effectScale = "not_available",
        requestedPercentChange = percentChange,
        candidatePredictors = predictorNames
      ),
      predictorResolution[c("warnings")]
    ))
  }

  logLogPredictor = matchLogLogUnitChangePredictor(
    logLog = logLog,
    predictorName = predictorResolution$predictorName
  )
  if (is.null(logLogPredictor)) {
    return(list(
      status = "unsupported",
      reason = "unsupported_log_log_structure",
      modelType = "lm",
      effectScale = "not_available",
      responseName = responseName,
      predictorName = predictorResolution$predictorName,
      requestedPercentChange = percentChange,
      warnings = "The requested proportional-change effect could not be matched to one log-log predictor."
    ))
  }

  computeLogLogProportionalChangeResult(
    model = model,
    responseName = logLog$responseVariable %||% responseName,
    transformedPredictorName = logLogPredictor$transformedName,
    originalPredictorName = logLogPredictor$originalName,
    percentPredictorChange = percentChange
  )
}

#' @keywords internal
#' @noRd
computeLogLogProportionalChangeResult = function(
    model,
    responseName,
    transformedPredictorName,
    originalPredictorName,
    percentPredictorChange) {
  coefValues = stats::coef(model)
  if (!(transformedPredictorName %in% names(coefValues))) {
    return(list(
      status = "unsupported",
      reason = "unsupported_log_log_structure",
      modelType = "lm",
      modelStructure = "log_log",
      effectScale = "not_available",
      responseName = responseName,
      predictorName = originalPredictorName,
      transformedPredictorName = transformedPredictorName,
      requestedPercentChange = percentPredictorChange,
      warnings = "The log-log proportional-change request does not have a simple one-coefficient log-predictor effect."
    ))
  }

  oneUnitEffect = unname(as.numeric(coefValues[[transformedPredictorName]]))
  proportionalPredictorChange = 1 + percentPredictorChange / 100
  logRatio = log(proportionalPredictorChange)
  logResponseChange = oneUnitEffect * logRatio
  responseMultiplier = exp(logResponseChange)
  responsePercentChange = 100 * (responseMultiplier - 1)

  ciResult = tryCatch(
    stats::confint(model, parm = transformedPredictorName),
    error = function(e) {
      NULL
    }
  )
  confidenceInterval = NULL
  ciValues = extractUnitChangeConfidenceLimits(ciResult)
  if (length(ciValues) == 2L) {
    lwr = exp(unname(ciValues[[1]]) * logRatio)
    upr = exp(unname(ciValues[[2]]) * logRatio)
    percentChangeLwr = 100 * (lwr - 1)
    percentChangeUpr = 100 * (upr - 1)
    confidenceInterval = list(
      level = 0.95,
      oneUnitLwr = unname(ciValues[[1]]),
      oneUnitUpr = unname(ciValues[[2]]),
      lwr = lwr,
      upr = upr,
      percentChangeLwr = percentChangeLwr,
      percentChangeUpr = percentChangeUpr,
      percentChangeIntervalText = formatUnitChangePercentIntervalText(
        percentChangeLwr = percentChangeLwr,
        percentChangeUpr = percentChangeUpr
      )
    )
  }

  list(
    status = "ok",
    modelType = "lm",
    modelStructure = "log_log",
    effectScale = "response_multiplier",
    responseName = responseName,
    predictorName = originalPredictorName,
    transformedPredictorName = transformedPredictorName,
    requestedUnitChange = NA_real_,
    requestedPercentChange = percentPredictorChange,
    referenceValue = 1,
    comparisonValue = proportionalPredictorChange,
    proportionalPredictorChange = proportionalPredictorChange,
    oneUnitEffect = oneUnitEffect,
    logRatio = logRatio,
    logResponseChange = logResponseChange,
    transformedEstimate = responseMultiplier,
    unitChangeEffect = responseMultiplier,
    percentChange = responsePercentChange,
    percentChangeText = formatUnitChangePercentText(responsePercentChange),
    percentChangeIntervalText = if (is.list(confidenceInterval)) {
      confidenceInterval$percentChangeIntervalText
    } else {
      NULL
    },
    confidenceInterval = confidenceInterval,
    interpretation = paste0(
      "For a ", signif(percentPredictorChange, 6), "% increase in ",
      originalPredictorName, ", the fitted ", responseName, " is ",
      formatUnitChangePercentText(responsePercentChange), "."
    )
  )
}

#' @keywords internal
#' @noRd
matchLogLogUnitChangePredictor = function(logLog, predictorName) {
  if (!is.list(logLog) || !isTRUE(logLog$isLogLog)) {
    return(NULL)
  }

  logPredictors = logLog$logPredictors
  if (!is.data.frame(logPredictors) || nrow(logPredictors) == 0) {
    return(NULL)
  }

  matches = logPredictors[
    logPredictors$transformedName == predictorName |
      logPredictors$termLabel == predictorName |
      logPredictors$originalName == predictorName,
    ,
    drop = FALSE
  ]

  if (nrow(matches) != 1L) {
    return(NULL)
  }

  as.list(matches[1, , drop = FALSE])
}

#' @keywords internal
#' @noRd
computeLogLogUnitChangeResult = function(
    model,
    modelFrame,
    responseName,
    transformedPredictorName,
    originalPredictorName,
    unitChange) {
  coefValues = stats::coef(model)
  if (!(transformedPredictorName %in% names(coefValues))) {
    return(list(
      status = "unsupported",
      reason = "unsupported_log_log_structure",
      modelType = "lm",
      effectScale = "not_available",
      responseName = responseName,
      predictorName = originalPredictorName,
      transformedPredictorName = transformedPredictorName,
      requestedUnitChange = unitChange,
      warnings = "The log-log unit-change request does not have a simple one-coefficient log-predictor effect."
    ))
  }

  transformedValues = suppressWarnings(as.numeric(modelFrame[[transformedPredictorName]]))
  transformedValues = transformedValues[is.finite(transformedValues)]
  if (length(transformedValues) == 0) {
    return(list(
      status = "unsupported",
      reason = "missing_log_predictor_values",
      modelType = "lm",
      effectScale = "not_available",
      responseName = responseName,
      predictorName = originalPredictorName,
      transformedPredictorName = transformedPredictorName,
      requestedUnitChange = unitChange,
      warnings = "WMFM could not recover finite fitted log-predictor values for this log-log unit-change request."
    ))
  }

  referenceValue = exp(mean(transformedValues))
  comparisonValue = referenceValue + unitChange
  if (!is.finite(referenceValue) || !is.finite(comparisonValue) || referenceValue <= 0 || comparisonValue <= 0) {
    return(list(
      status = "unsupported",
      reason = "invalid_original_scale_reference",
      modelType = "lm",
      effectScale = "not_available",
      responseName = responseName,
      predictorName = originalPredictorName,
      transformedPredictorName = transformedPredictorName,
      requestedUnitChange = unitChange,
      warnings = "The requested original-scale change could not be evaluated from a positive reference value."
    ))
  }

  oneUnitEffect = unname(as.numeric(coefValues[[transformedPredictorName]]))
  logRatio = log(comparisonValue / referenceValue)
  logResponseChange = oneUnitEffect * logRatio
  responseMultiplier = exp(logResponseChange)
  percentChange = 100 * (responseMultiplier - 1)

  ciResult = tryCatch(
    stats::confint(model, parm = transformedPredictorName),
    error = function(e) {
      NULL
    }
  )
  confidenceInterval = NULL
  ciValues = extractUnitChangeConfidenceLimits(ciResult)
  if (length(ciValues) == 2L) {
    lwr = exp(unname(ciValues[[1]]) * logRatio)
    upr = exp(unname(ciValues[[2]]) * logRatio)
    percentChangeLwr = 100 * (lwr - 1)
    percentChangeUpr = 100 * (upr - 1)
    confidenceInterval = list(
      level = 0.95,
      oneUnitLwr = unname(ciValues[[1]]),
      oneUnitUpr = unname(ciValues[[2]]),
      lwr = lwr,
      upr = upr,
      percentChangeLwr = percentChangeLwr,
      percentChangeUpr = percentChangeUpr,
      percentChangeIntervalText = formatUnitChangePercentIntervalText(
        percentChangeLwr = percentChangeLwr,
        percentChangeUpr = percentChangeUpr
      )
    )
  }

  list(
    status = "ok",
    modelType = "lm",
    modelStructure = "log_log",
    effectScale = "response_multiplier",
    responseName = responseName,
    predictorName = originalPredictorName,
    transformedPredictorName = transformedPredictorName,
    requestedUnitChange = unitChange,
    referenceValue = referenceValue,
    comparisonValue = comparisonValue,
    proportionalPredictorChange = comparisonValue / referenceValue,
    oneUnitEffect = oneUnitEffect,
    logRatio = logRatio,
    logResponseChange = logResponseChange,
    transformedEstimate = responseMultiplier,
    unitChangeEffect = responseMultiplier,
    percentChange = percentChange,
    percentChangeText = formatUnitChangePercentText(percentChange),
    percentChangeIntervalText = if (is.list(confidenceInterval)) {
      confidenceInterval$percentChangeIntervalText
    } else {
      NULL
    },
    confidenceInterval = confidenceInterval,
    interpretation = paste0(
      "Starting from a typical ", originalPredictorName, " value of ",
      signif(referenceValue, 3), ", increasing to ",
      signif(comparisonValue, 3), " ", originalPredictorName,
      " (a ", signif(unitChange, 3), "-", originalPredictorName,
      " increase) is associated with a fitted ",
      parseNaturalLogCall(responseName) %||% responseName,
      " that is ", formatUnitChangePercentText(percentChange), "."
    )
  )
}

#' @keywords internal
#' @noRd
computeGlmUnitChangeResult = function(model, responseName, predictorName, unitChange, oneUnitEffect, ciResult = NULL) {
  familyName = model$family$family %||% ""
  linkName = model$family$link %||% ""

  if (identical(familyName, "poisson") && identical(linkName, "log")) {
    effectScale = "expected_count_multiplier"
    interpretationNoun = "expected count"
  } else if (identical(familyName, "binomial") && identical(linkName, "logit")) {
    effectScale = "odds_multiplier"
    interpretationNoun = "odds"
  } else {
    return(list(
      status = "unsupported",
      reason = "unsupported_glm_family_link",
      modelType = "glm",
      glmFamily = familyName,
      glmLink = linkName,
      effectScale = "not_available",
      responseName = responseName,
      predictorName = predictorName,
      requestedUnitChange = unitChange,
      warnings = "Only Poisson log-link and binomial logit-link unit-change effects are currently supported."
    ))
  }

  linkScaleEffect = oneUnitEffect * unitChange
  transformedEstimate = exp(linkScaleEffect)
  percentChange = 100 * (transformedEstimate - 1)
  confidenceInterval = NULL
  ciValues = extractUnitChangeConfidenceLimits(ciResult)
  if (length(ciValues) == 2L) {
    oneUnitLwr = unname(ciValues[[1]])
    oneUnitUpr = unname(ciValues[[2]])
    lwr = exp(oneUnitLwr * unitChange)
    upr = exp(oneUnitUpr * unitChange)
    percentChangeLwr = 100 * (lwr - 1)
    percentChangeUpr = 100 * (upr - 1)
    confidenceInterval = list(
      level = 0.95,
      oneUnitLwr = oneUnitLwr,
      oneUnitUpr = oneUnitUpr,
      lwr = lwr,
      upr = upr,
      percentChangeLwr = percentChangeLwr,
      percentChangeUpr = percentChangeUpr,
      percentChangeIntervalText = formatUnitChangePercentIntervalText(
        percentChangeLwr = percentChangeLwr,
        percentChangeUpr = percentChangeUpr
      )
    )
  }

  list(
    status = "ok",
    modelType = "glm",
    glmFamily = familyName,
    glmLink = linkName,
    effectScale = effectScale,
    responseName = responseName,
    predictorName = predictorName,
    requestedUnitChange = unitChange,
    oneUnitEffect = oneUnitEffect,
    linkScaleUnitChangeEffect = linkScaleEffect,
    transformedEstimate = transformedEstimate,
    unitChangeEffect = transformedEstimate,
    percentChange = percentChange,
    percentChangeText = formatUnitChangePercentText(percentChange),
    percentChangeIntervalText = if (is.list(confidenceInterval)) {
      confidenceInterval$percentChangeIntervalText
    } else {
      NULL
    },
    confidenceInterval = confidenceInterval,
    interpretation = paste0(
      "For a ", signif(unitChange, 6), "-unit increase in ", predictorName,
      ", the ", interpretationNoun, " is ",
      formatUnitChangePercentText(percentChange), "."
    )
  )
}

#' @keywords internal
#' @noRd
formatUnitChangePercentText = function(percentChange) {
  percentChange = suppressWarnings(as.numeric(percentChange))
  if (length(percentChange) != 1L || !is.finite(percentChange)) {
    return("changed by an unavailable percentage")
  }

  roundedChange = signif(abs(percentChange), 3)
  if (abs(percentChange) < .Machine$double.eps^0.5) {
    return("changed by about 0%")
  }

  if (percentChange > 0) {
    return(paste0("about ", roundedChange, "% higher"))
  }

  paste0("about ", roundedChange, "% lower")
}

#' @keywords internal
#' @noRd
formatUnitChangePercentIntervalText = function(percentChangeLwr, percentChangeUpr) {
  values = suppressWarnings(as.numeric(c(percentChangeLwr, percentChangeUpr)))
  if (length(values) != 2L || any(!is.finite(values))) {
    return("changed by an unavailable percentage interval")
  }

  if (all(values <= 0)) {
    absValues = sort(abs(values))
    return(paste0(
      "between about ", signif(absValues[[1]], 3), "% and ",
      signif(absValues[[2]], 3), "% lower"
    ))
  }

  if (all(values >= 0)) {
    sortedValues = sort(values)
    return(paste0(
      "between about ", signif(sortedValues[[1]], 3), "% and ",
      signif(sortedValues[[2]], 3), "% higher"
    ))
  }

  sortedValues = sort(values)
  paste0(
    "between about ", signif(abs(sortedValues[[1]]), 3),
    "% lower and ", signif(sortedValues[[2]], 3), "% higher"
  )
}

#' @keywords internal
#' @noRd
extractUnitChangeConfidenceLimits = function(ciResult = NULL) {
  if (is.null(ciResult)) {
    return(numeric(0))
  }

  values = suppressWarnings(as.numeric(ciResult))
  values = values[is.finite(values)]
  if (length(values) < 2L) {
    return(numeric(0))
  }

  unname(values[1:2])
}

#' @keywords internal
#' @noRd
resolveRequestedUnitChange = function(requestedUnitChange = numeric(0)) {
  values = suppressWarnings(as.numeric(requestedUnitChange %||% numeric(0)))
  values = values[is.finite(values)]
  values = unique(values)
  if (length(values) == 0) {
    return(NA_real_)
  }
  if (length(values) > 1) {
    return(NA_real_)
  }
  values[[1]]
}

#' @keywords internal
#' @noRd
resolveUnitChangePredictor = function(followupQuestion, numericPredictors) {
  text = tolower(trimws(as.character(followupQuestion %||% "")))
  if (!nzchar(text)) {
    text = ""
  }

  matches = vapply(numericPredictors, function(predictorName) {
    escapedName = escapeRegexLiteral(tolower(predictorName))
    grepl(paste0("(?<![a-z0-9_.])", escapedName, "(?![a-z0-9_.])"), text, perl = TRUE)
  }, logical(1))

  matchedPredictors = numericPredictors[matches]
  if (length(matchedPredictors) == 1L) {
    return(list(ok = TRUE, predictorName = matchedPredictors[[1]]))
  }

  if (length(matchedPredictors) > 1L) {
    return(list(
      ok = FALSE,
      status = "clarification_required",
      reason = "ambiguous_predictor",
      warnings = paste0(
        "The unit-change request mentions multiple numeric predictors: ",
        paste(matchedPredictors, collapse = ", "),
        ". Please name exactly one predictor."
      )
    ))
  }

  if (length(numericPredictors) == 1L) {
    return(list(ok = TRUE, predictorName = numericPredictors[[1]]))
  }

  list(
    ok = FALSE,
    status = "clarification_required",
    reason = "ambiguous_predictor",
    warnings = paste0(
      "The fitted model has multiple numeric predictors: ",
      paste(numericPredictors, collapse = ", "),
      ". Please name the predictor for the unit-change interpretation."
    )
  )
}

#' @keywords internal
#' @noRd
escapeRegexLiteral = function(x) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x, perl = TRUE)
}
