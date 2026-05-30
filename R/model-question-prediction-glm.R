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

  if (isTRUE(requestsPredictionInterval)) {
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
        predictionIntervalUnsupportedReason = "GLM follow-up prediction intervals for a future observation are not currently supported; WMFM reports a confidence interval for the fitted mean response instead.",
        warnings = paste(
          "Deterministic GLM prediction intervals for a future observation are not currently supported in Stage 25.",
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

  payload = formatModelQuestionPredictionPayload(
    modelType = "glm",
    predictionType = "mean_response_prediction",
    responseScale = responseScale,
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    completedPredictorValues = newDataInfo$completedPredictorValues,
    fittedPrediction = fittedPrediction,
    confidenceInterval = confidenceInterval,
    predictionInterval = NULL,
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
  payload$predictionIntervalUnsupportedReason = "GLM follow-up prediction intervals for a future observation are not currently supported; WMFM reports a confidence interval for the fitted mean response instead."
  payload
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
      classification = classification
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

  list(
    status = status,
    thresholdFraction = thresholdFraction,
    numericPredictors = numericDiagnostics,
    message = message
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
