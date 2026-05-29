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
      confidenceIntervalSupported = FALSE,
      confidenceIntervalUnsupportedReason = "GLM confidence intervals require a fitted glm model.",
      predictionIntervalSupported = FALSE,
      predictionIntervalUnsupportedReason = "GLM prediction intervals are not currently supported deterministically.",
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
      confidenceIntervalSupported = FALSE,
      confidenceIntervalUnsupportedReason = "GLM confidence intervals are currently supported only for binomial logistic and Poisson models in this deterministic pathway.",
      predictionIntervalSupported = FALSE,
      predictionIntervalUnsupportedReason = "GLM prediction intervals are not currently supported deterministically.",
      warnings = "This stage supports deterministic GLM mean predictions only for binomial logistic and Poisson models."
    ))
  }

  lowerText = tolower(as.character(followupQuestion %||% ""))
  requestsPredictionInterval = grepl("\\bprediction intervals?\\b", lowerText, perl = TRUE)
  requestsConfidenceInterval = grepl("\\bconfidence intervals?\\b", lowerText, perl = TRUE)

  inputValidation = validateLmPredictionInputs(
    model = model,
    followupQuestion = followupQuestion,
    allowMissingPredictorCompletion = allowMissingPredictorCompletion
  )

  if (isTRUE(requestsPredictionInterval)) {
    return(c(
      list(
        status = "unsupported",
        reason = "unsupported_glm_prediction_interval",
        modelType = "glm",
        glmFamily = familyName,
        glmLink = linkName,
        predictionType = "individual_prediction_interval",
        responseScale = "response",
        predictionInterval = NULL,
        predictionIntervalSupported = FALSE,
        predictionIntervalUnsupportedReason = paste(
          "GLM follow-up prediction intervals are not currently supported deterministically.",
          "For binomial and Poisson GLMs, WMFM can compute a confidence interval for the mean response on the response scale,",
          "but an individual prediction interval requires a distribution-specific predictive calculation that is not implemented in Stage 25.3."
        )
      ),
      inputValidation[c("suppliedPredictorValues", "requiredPredictors")],
      list(
        warnings = paste(
          "GLM prediction interval request was not computed.",
          "Use a mean-response prediction or confidence interval follow-up instead."
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

  fittedPrediction = as.numeric(stats::predict(model, newdata = newDataInfo$newData, type = "response"))[1]
  confidenceInterval = computeGlmMeanResponseConfidenceInterval(
    model = model,
    newData = newDataInfo$newData
  )
  predictionType = if (isTRUE(requestsConfidenceInterval)) {
    "confidence_interval_for_mean_response"
  } else {
    "mean_response_prediction"
  }
  predictionIntervalUnsupportedReason = paste(
    "GLM follow-up prediction intervals are not currently supported deterministically.",
    "For binomial and Poisson GLMs, this payload reports the fitted mean response and its confidence interval on the response scale instead."
  )
  responseDescription = if (identical(familyName, "binomial")) {
    "probability"
  } else if (identical(familyName, "poisson")) {
    "expected_count"
  } else {
    "mean_response"
  }

  payload = formatModelQuestionPredictionPayload(
    modelType = "glm",
    predictionType = predictionType,
    responseScale = "response",
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    completedPredictorValues = newDataInfo$completedPredictorValues,
    fittedPrediction = fittedPrediction,
    confidenceInterval = confidenceInterval,
    predictionInterval = NULL,
    warnings = c(
      sprintf("Computed with stats::predict(type = 'response') for %s GLM.", familyName),
      "GLM mean-response confidence interval computed with stats::predict(type = 'link', se.fit = TRUE), then back-transformed to the response scale.",
      newDataInfo$warnings
    )
  )
  payload$glmFamily = familyName
  payload$glmLink = linkName
  payload$responseDescription = responseDescription
  payload$confidenceIntervalSupported = is.list(confidenceInterval)
  payload$predictionIntervalSupported = FALSE
  payload$predictionIntervalUnsupportedReason = predictionIntervalUnsupportedReason
  payload
}

#' Compute a GLM mean-response confidence interval on the response scale
#'
#' Uses the fitted model's link function for the standard-error calculation,
#' then back-transforms the interval endpoints with the inverse link.
#'
#' @param model Fitted glm model.
#' @param newData One-row prediction data frame.
#'
#' @return Named list with fit/lwr/upr/level, or NULL if unavailable.
#' @keywords internal
#' @noRd
computeGlmMeanResponseConfidenceInterval = function(model, newData) {
  pred = stats::predict(model, newdata = newData, type = "link", se.fit = TRUE)
  fit = as.numeric(pred$fit)[1]
  se = as.numeric(pred$se.fit)[1]
  if (!is.finite(fit) || !is.finite(se)) {
    return(NULL)
  }

  level = 0.95
  alpha = 1 - level
  z = stats::qnorm(1 - alpha / 2)
  invLink = model$family$linkinv
  list(
    fit = as.numeric(invLink(fit)),
    lwr = as.numeric(invLink(fit - z * se)),
    upr = as.numeric(invLink(fit + z * se)),
    level = level,
    scale = "response",
    method = "link_se_back_transform"
  )
}
