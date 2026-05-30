#' Compute deterministic generalised-linear-model follow-up prediction payload
#'
#' @param model Fitted model object.
#' @param followupQuestion Character scalar bounded follow-up question.
#'
#' @return Named list prediction payload.
#' @keywords internal
#' @noRd
computeGlmModelQuestionPrediction = function(model, followupQuestion, allowMissingPredictorCompletion = TRUE) {
  if (!inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_type",
      modelType = class(model)[[1]],
      predictionType = "mean_response_prediction",
      responseScale = "response",
      warnings = "GLM deterministic prediction pathway requires a fitted glm model."
    ))
  }

  familyName = tolower(model$family$family %||% "")
  linkName = tolower(model$family$link %||% "")
  if (!(familyName %in% c("binomial", "poisson"))) {
    return(list(
      status = "unsupported",
      reason = "unsupported_glm_family",
      modelType = "glm",
      glmFamily = familyName,
      glmLink = linkName,
      predictionType = "mean_response_prediction",
      responseScale = "response",
      warnings = "This stage supports deterministic GLM mean predictions only for binomial logistic and Poisson models."
    ))
  }

  lowerText = tolower(as.character(followupQuestion %||% ""))
  requestsPredictionInterval = grepl("\\bprediction intervals?\\b", lowerText, perl = TRUE)
  requestsConfidenceInterval = grepl("\\bconfidence intervals?\\b", lowerText, perl = TRUE)
  requestsOddsScale = identical(familyName, "binomial") &&
    identical(linkName, "logit") &&
    grepl("\\bodds\\b", lowerText, perl = TRUE)

  inputValidation = validateLmPredictionInputs(
    model = model,
    followupQuestion = followupQuestion,
    allowMissingPredictorCompletion = allowMissingPredictorCompletion
  )

  predictionIntervalPolicy = buildGlmPredictionIntervalPolicy(
    familyName = familyName,
    linkName = linkName,
    requestedPredictionInterval = requestsPredictionInterval
  )

  if (isTRUE(requestsPredictionInterval) && !isTRUE(predictionIntervalPolicy$supported)) {
    return(c(
      list(
        status = "unsupported",
        reason = "unsupported_glm_interval_request",
        modelType = "glm",
        glmFamily = familyName,
        glmLink = linkName,
        predictionType = "individual_prediction_interval",
        responseScale = "response"
      ),
      inputValidation[c("suppliedPredictorValues", "requiredPredictors")],
      list(
        predictionIntervalPolicy = predictionIntervalPolicy,
        predictionIntervalUnsupportedReason = predictionIntervalPolicy$studentExplanation,
        warnings = paste(
          "Deterministic GLM prediction intervals for this future-observation type are not currently supported in Stage 27.5.",
          "Use the fitted mean-response prediction and its confidence interval instead."
        )
      )
    ))
  }

  if (!isTRUE(inputValidation$ok)) {
    return(c(
      list(
        status = inputValidation$status %||% ifelse(identical(inputValidation$reason, "clarification_required"), "clarification_required", "needs_input"),
        reason = inputValidation$reason,
        modelType = "glm",
        glmFamily = familyName,
        glmLink = linkName,
        predictionType = "mean_response_prediction",
        responseScale = "response"
      ),
      inputValidation[c("suppliedPredictorValues", "requiredPredictors", "warnings")]
    ))
  }

  suppliedNames = names(inputValidation$suppliedPredictorValues %||% list())
  missingPredictors = setdiff(inputValidation$requiredPredictors %||% character(0), suppliedNames)
  if (length(missingPredictors) > 0 && !isTRUE(allowMissingPredictorCompletion)) {
    return(list(
      status = "needs_input",
      reason = "missing_predictor_values",
      modelType = "glm",
      glmFamily = familyName,
      glmLink = linkName,
      predictionType = "mean_response_prediction",
      responseScale = "response",
      suppliedPredictorValues = inputValidation$suppliedPredictorValues,
      requiredPredictors = inputValidation$requiredPredictors,
      missingPredictors = missingPredictors,
      warnings = paste0(
        "Missing fitted-model predictor values: ",
        paste(missingPredictors, collapse = ", "),
        "."
      )
    ))
  }

  newDataInfo = buildLmPredictionNewData(model = model, suppliedPredictorValues = inputValidation$suppliedPredictorValues)
  if (!isTRUE(newDataInfo$ok)) {
    return(c(
      list(
        status = "needs_input",
        reason = newDataInfo$reason,
        modelType = "glm",
        glmFamily = familyName,
        glmLink = linkName,
        predictionType = "mean_response_prediction",
        responseScale = "response"
      ),
      newDataInfo[c("suppliedPredictorValues", "requiredPredictors", "warnings")]
    ))
  }

  extrapolationPolicy = classifyGlmFollowupExtrapolation(
    model = model,
    suppliedPredictorValues = inputValidation$suppliedPredictorValues
  )
  if (identical(extrapolationPolicy$status, "extrapolation_blocked")) {
    return(list(
      status = "extrapolation_blocked",
      reason = "extrapolation_blocked",
      modelType = "glm",
      glmFamily = familyName,
      glmLink = linkName,
      predictionType = "mean_response_prediction",
      responseScale = "response",
      suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
      resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
      completedPredictorValues = newDataInfo$completedPredictorValues,
      extrapolationPolicy = extrapolationPolicy,
      extrapolationDiagnostics = extrapolationPolicy$diagnostics,
      extrapolationExplanation = extrapolationPolicy$explanationText,
      warnings = extrapolationPolicy$message
    ))
  }

  linkPrediction = stats::predict(model, newdata = newDataInfo$newData, type = "link", se.fit = TRUE)
  linkFit = as.numeric(linkPrediction$fit)[1]
  linkSe = as.numeric(linkPrediction$se.fit)[1]
  responseScale = if (isTRUE(requestsOddsScale)) "odds" else "response"
  fittedPrediction = if (isTRUE(requestsOddsScale)) {
    exp(linkFit)
  } else {
    as.numeric(model$family$linkinv(linkFit))[1]
  }
  confidenceInterval = NULL
  if (is.finite(linkFit) && is.finite(linkSe)) {
    linkLwr = linkFit - stats::qnorm(0.975) * linkSe
    linkUpr = linkFit + stats::qnorm(0.975) * linkSe
    if (isTRUE(requestsOddsScale)) {
      confidenceInterval = list(
        fit = fittedPrediction,
        lwr = exp(linkLwr),
        upr = exp(linkUpr),
        level = 0.95,
        scale = "odds",
        intervalScale = "odds",
        method = "link_scale_exponentiate"
      )
    } else {
      confidenceInterval = list(
        fit = fittedPrediction,
        lwr = as.numeric(model$family$linkinv(linkLwr))[1],
        upr = as.numeric(model$family$linkinv(linkUpr))[1],
        level = 0.95,
        scale = "response",
        intervalScale = "response",
        method = "link_scale_delta_back_transform"
      )
    }
  }
  responseDescription = if (isTRUE(requestsOddsScale)) {
    "odds"
  } else if (identical(familyName, "binomial")) {
    "probability"
  } else if (identical(familyName, "poisson")) {
    "expected_count"
  } else {
    "mean_response"
  }

  predictionInterval = buildGlmFutureObservationPredictionInterval(
    model = model,
    familyName = familyName,
    fittedPrediction = fittedPrediction,
    requestedPredictionInterval = requestsPredictionInterval,
    predictionIntervalPolicy = predictionIntervalPolicy
  )

  payload = formatModelQuestionPredictionPayload(
    modelType = "glm",
    predictionType = "mean_response_prediction",
    responseScale = responseScale,
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    completedPredictorValues = newDataInfo$completedPredictorValues,
    fittedPrediction = fittedPrediction,
    confidenceInterval = confidenceInterval,
    predictionInterval = predictionInterval,
    warnings = c(
      if (isTRUE(requestsOddsScale)) {
        sprintf("Computed with stats::predict(type = 'link') and exponentiated to the odds scale for %s GLM.", familyName)
      } else {
        sprintf("Computed with stats::predict(type = 'response') for %s GLM.", familyName)
      },
      if (identical(extrapolationPolicy$status, "extrapolation_warning")) {
        extrapolationPolicy$message
      },
      newDataInfo$warnings
    )
  )
  payload$glmFamily = familyName
  payload$glmLink = linkName
  payload$responseDescription = responseDescription
  payload$extrapolationPolicy = extrapolationPolicy
  payload$extrapolationDiagnostics = extrapolationPolicy$diagnostics
  payload$extrapolationExplanation = extrapolationPolicy$explanationText
  payload$predictionIntervalPolicy = predictionIntervalPolicy
  payload$predictionIntervalUnsupportedReason = if (isTRUE(predictionIntervalPolicy$supported)) {
    NULL
  } else {
    predictionIntervalPolicy$studentExplanation
  }
  payload
}


#' Build GLM future-observation uncertainty metadata when supported
#'
#' @param model Fitted GLM object.
#' @param familyName Character scalar GLM family name.
#' @param fittedPrediction Numeric scalar fitted response-scale prediction.
#' @param requestedPredictionInterval Logical scalar indicating whether the user
#'   explicitly requested a prediction interval.
#' @param predictionIntervalPolicy Prediction-interval policy metadata.
#'
#' @return Named list prediction interval or future-outcome metadata, or NULL
#'   when no metadata should be attached to the payload.
#' @keywords internal
#' @noRd
buildGlmFutureObservationPredictionInterval = function(
    model,
    familyName,
    fittedPrediction,
    requestedPredictionInterval,
    predictionIntervalPolicy) {
  if (!isTRUE(requestedPredictionInterval) || !isTRUE(predictionIntervalPolicy$supported)) {
    return(NULL)
  }

  familyName = tolower(as.character(familyName %||% ""))
  fittedPrediction = suppressWarnings(as.numeric(fittedPrediction)[1])

  if (identical(familyName, "poisson")) {
    if (!is.finite(fittedPrediction) || fittedPrediction < 0) {
      return(NULL)
    }

    level = 0.95
    alpha = 1 - level
    return(list(
      fit = fittedPrediction,
      lwr = stats::qpois(alpha / 2, lambda = fittedPrediction),
      upr = stats::qpois(1 - alpha / 2, lambda = fittedPrediction),
      level = level,
      scale = "count",
      intervalScale = "count",
      distribution = "poisson",
      method = "conditional_poisson_quantile",
      parameterUncertaintyIncluded = FALSE
    ))
  }

  if (identical(familyName, "binomial")) {
    return(buildLogisticFutureOutcomeFraming(
      model = model,
      fittedProbability = fittedPrediction
    ))
  }

  NULL
}

#' Build deterministic Bernoulli future-outcome framing for logistic GLMs
#'
#' @param model Fitted GLM object.
#' @param fittedProbability Numeric scalar fitted probability for the modelled
#'   event.
#'
#' @return Named list with future binary outcome probabilities.
#' @keywords internal
#' @noRd
buildLogisticFutureOutcomeFraming = function(model, fittedProbability) {
  fittedProbability = suppressWarnings(as.numeric(fittedProbability)[1])
  if (!is.finite(fittedProbability)) {
    return(NULL)
  }

  fittedProbability = min(max(fittedProbability, 0), 1)
  outcomeLevels = getLogisticFutureOutcomeLevels(model)
  eventLevel = outcomeLevels$eventLevel
  otherLevel = outcomeLevels$otherLevel

  list(
    fit = fittedProbability,
    lwr = 0,
    upr = 1,
    level = NA_real_,
    scale = "binary_outcome",
    intervalScale = "binary_outcome",
    distribution = "bernoulli",
    method = "bernoulli_outcome_framing",
    parameterUncertaintyIncluded = FALSE,
    eventLevel = eventLevel,
    otherLevel = otherLevel,
    outcomeProbabilities = buildLogisticOutcomeProbabilityList(
      fittedProbability = fittedProbability,
      otherLevel = otherLevel,
      eventLevel = eventLevel
    ),
    explanation = paste(
      "An individual future outcome is binary, so WMFM reports the two possible outcomes and their fitted probabilities rather than a continuous prediction interval.",
      "The fitted probability is treated as fixed in this deterministic framing."
    )
  )
}

#' Build named probabilities for logistic future-outcome framing
#'
#' @param fittedProbability Numeric scalar fitted probability for the modelled
#'   event.
#' @param otherLevel Character scalar display label for the non-event outcome.
#' @param eventLevel Character scalar display label for the event outcome.
#'
#' @return Named list of outcome probabilities, including numeric aliases and
#'   response-level labels when those labels differ from the numeric aliases.
#' @keywords internal
#' @noRd
buildLogisticOutcomeProbabilityList = function(fittedProbability, otherLevel, eventLevel) {
  probabilities = list(
    "0" = 1 - fittedProbability,
    "1" = fittedProbability
  )

  if (!identical(otherLevel, "0")) {
    probabilities[[otherLevel]] = 1 - fittedProbability
  }

  if (!identical(eventLevel, "1")) {
    probabilities[[eventLevel]] = fittedProbability
  }

  probabilities
}

#' Resolve display labels for logistic future-outcome framing
#'
#' @param model Fitted GLM object.
#'
#' @return Named list with `otherLevel` and `eventLevel` labels.
#' @keywords internal
#' @noRd
getLogisticFutureOutcomeLevels = function(model) {
  mf = stats::model.frame(model)
  response = mf[[1]]

  if (is.factor(response) && length(levels(response)) == 2) {
    return(list(
      otherLevel = levels(response)[[1]],
      eventLevel = levels(response)[[2]]
    ))
  }

  if (is.logical(response)) {
    return(list(
      otherLevel = "FALSE",
      eventLevel = "TRUE"
    ))
  }

  list(
    otherLevel = "0",
    eventLevel = "1"
  )
}

#' Classify extrapolation for GLM follow-up prediction values
#'
#' @param model Fitted GLM object.
#' @param suppliedPredictorValues Named list of values parsed from the follow-up
#'   question. Only explicitly supplied numeric predictors are classified.
#' @param thresholdFraction Numeric scalar giving the proportion of the observed
#'   range width allowed beyond the nearest boundary before suppressing the
#'   prediction.
#'
#' @return Named list describing the overall extrapolation status and per-predictor
#'   diagnostics.
#' @keywords internal
#' @noRd
classifyGlmFollowupExtrapolation = function(model, suppliedPredictorValues, thresholdFraction = 0.10) {
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  suppliedPredictorValues = suppliedPredictorValues %||% list()
  numericDiagnostics = list()

  for (predictorName in predictorNames) {
    column = mf[[predictorName]]
    suppliedValue = suppliedPredictorValues[[predictorName]]
    valueWasSupplied = !is.null(suppliedValue) && nzchar(trimws(as.character(suppliedValue)))

    if (!isTRUE(valueWasSupplied) || !is.numeric(column)) {
      next
    }

    requestedValue = suppressWarnings(as.numeric(suppliedValue))
    observedMin = suppressWarnings(min(column, na.rm = TRUE))
    observedMax = suppressWarnings(max(column, na.rm = TRUE))

    if (!is.finite(requestedValue) || !is.finite(observedMin) || !is.finite(observedMax)) {
      next
    }

    observedWidth = observedMax - observedMin
    tolerance = max(observedWidth * thresholdFraction, 0)
    nearestBoundary = NA_real_
    distanceOutside = 0
    classification = "in_range"

    if (requestedValue < observedMin) {
      nearestBoundary = observedMin
      distanceOutside = observedMin - requestedValue
    } else if (requestedValue > observedMax) {
      nearestBoundary = observedMax
      distanceOutside = requestedValue - observedMax
    }

    if (distanceOutside > 0) {
      classification = if (distanceOutside <= tolerance) {
        "extrapolation_warning"
      } else {
        "extrapolation_blocked"
      }
    }

    numericDiagnostics[[predictorName]] = list(
      predictor = predictorName,
      observedMin = observedMin,
      observedMax = observedMax,
      requestedValue = requestedValue,
      nearestBoundary = nearestBoundary,
      distanceOutside = distanceOutside,
      tolerance = tolerance,
      classification = classification,
      observedRange = list(
        lower = observedMin,
        upper = observedMax,
        width = observedWidth
      ),
      explanatoryText = buildGlmExtrapolationPredictorText(
        predictorName = predictorName,
        requestedValue = requestedValue,
        observedMin = observedMin,
        observedMax = observedMax,
        distanceOutside = distanceOutside,
        tolerance = tolerance,
        classification = classification
      )
    )
  }

  classifications = vapply(numericDiagnostics, function(x) {
    x$classification %||% "in_range"
  }, character(1))

  status = if (any(classifications == "extrapolation_blocked")) {
    "extrapolation_blocked"
  } else if (any(classifications == "extrapolation_warning")) {
    "extrapolation_warning"
  } else {
    "in_range"
  }

  message = buildGlmExtrapolationMessage(status = status, numericDiagnostics = numericDiagnostics)
  diagnostics = buildGlmExtrapolationDiagnostics(
    status = status,
    thresholdFraction = thresholdFraction,
    numericDiagnostics = numericDiagnostics,
    message = message
  )

  list(
    status = status,
    thresholdFraction = thresholdFraction,
    numericPredictors = numericDiagnostics,
    diagnostics = diagnostics,
    explanationText = diagnostics$explanationText,
    message = message
  )
}

#' Build per-predictor GLM extrapolation explanatory text
#'
#' @param predictorName Character scalar predictor name.
#' @param requestedValue Numeric scalar requested value.
#' @param observedMin Numeric scalar observed minimum.
#' @param observedMax Numeric scalar observed maximum.
#' @param distanceOutside Numeric scalar distance outside the observed range.
#' @param tolerance Numeric scalar extrapolation warning tolerance.
#' @param classification Character scalar extrapolation classification.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
buildGlmExtrapolationPredictorText = function(
    predictorName,
    requestedValue,
    observedMin,
    observedMax,
    distanceOutside,
    tolerance,
    classification) {
  if (identical(classification, "in_range")) {
    return(sprintf(
      "%s = %.6g is inside the observed range [%.6g, %.6g].",
      predictorName,
      requestedValue,
      observedMin,
      observedMax
    ))
  }

  policyText = if (identical(classification, "extrapolation_blocked")) {
    "This is beyond the configured extrapolation tolerance, so WMFM suppresses the deterministic prediction."
  } else {
    "This is outside the observed range but within the configured extrapolation tolerance, so WMFM reports the prediction with a warning."
  }

  sprintf(
    paste(
      "%s = %.6g is outside the observed range [%.6g, %.6g].",
      "It is %.6g units beyond the nearest boundary; the configured tolerance is %.6g.",
      "%s"
    ),
    predictorName,
    requestedValue,
    observedMin,
    observedMax,
    distanceOutside,
    tolerance,
    policyText
  )
}

#' Build machine-readable GLM extrapolation diagnostics
#'
#' @param status Overall extrapolation status.
#' @param thresholdFraction Numeric scalar extrapolation threshold fraction.
#' @param numericDiagnostics Per-predictor extrapolation diagnostics.
#' @param message Character scalar plain-language policy message.
#'
#' @return Named list of stable diagnostics fields.
#' @keywords internal
#' @noRd
buildGlmExtrapolationDiagnostics = function(status, thresholdFraction, numericDiagnostics, message) {
  predictorDiagnostics = lapply(numericDiagnostics, function(x) {
    list(
      predictor = x$predictor,
      observedRange = x$observedRange,
      requestedValue = x$requestedValue,
      nearestBoundary = x$nearestBoundary,
      distanceOutside = x$distanceOutside,
      tolerance = x$tolerance,
      classification = x$classification,
      explanatoryText = x$explanatoryText
    )
  })

  list(
    status = status,
    thresholdFraction = thresholdFraction,
    numericPredictors = predictorDiagnostics,
    observedRanges = lapply(predictorDiagnostics, function(x) {
      x$observedRange
    }),
    requestedValues = lapply(predictorDiagnostics, function(x) {
      x$requestedValue
    }),
    classifications = lapply(predictorDiagnostics, function(x) {
      x$classification
    }),
    explanationText = message
  )
}

#' Build a plain-language GLM extrapolation message
#'
#' @param status Overall extrapolation status.
#' @param numericDiagnostics Per-predictor extrapolation diagnostics.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
buildGlmExtrapolationMessage = function(status, numericDiagnostics) {
  if (identical(status, "in_range")) {
    return("All supplied numeric predictor values are inside the observed data range.")
  }

  affected = numericDiagnostics[vapply(numericDiagnostics, function(x) {
    !identical(x$classification %||% "in_range", "in_range")
  }, logical(1))]

  parts = vapply(affected, function(x) {
    sprintf(
      "%s = %.6g is outside the observed range [%.6g, %.6g]",
      x$predictor,
      x$requestedValue,
      x$observedMin,
      x$observedMax
    )
  }, character(1))

  prefix = if (identical(status, "extrapolation_blocked")) {
    "WMFM suppressed this GLM follow-up prediction because it requires extrapolation beyond the configured range policy:"
  } else {
    "WMFM computed this GLM follow-up prediction, but it uses slight extrapolation beyond the observed data range:"
  }

  paste(prefix, paste(parts, collapse = "; "))
}
