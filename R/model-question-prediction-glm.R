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

  inputValidation = validateLmPredictionInputs(
    model = model,
    followupQuestion = followupQuestion,
    allowMissingPredictorCompletion = allowMissingPredictorCompletion
  )


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
  confidenceInterval = if (isTRUE(requestsConfidenceInterval)) {
    computeGlmMeanResponseConfidenceInterval(model = model, newData = newDataInfo$newData)
  } else {
    NULL
  }
  predictionIntervalUnsupportedReason = if (isTRUE(requestsPredictionInterval)) {
    "GLM prediction intervals for individual outcomes are not currently supported by the deterministic follow-up pathway."
  } else {
    NULL
  }
  responseDescription = if (identical(familyName, "binomial")) {
    "probability"
  } else if (identical(familyName, "poisson")) {
    "expected_count"
  } else {
    "mean_response"
  }

  payload = formatModelQuestionPredictionPayload(
    modelType = "glm",
    predictionType = "mean_response_prediction",
    responseScale = "response",
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    completedPredictorValues = newDataInfo$completedPredictorValues,
    fittedPrediction = fittedPrediction,
    confidenceInterval = confidenceInterval,
    predictionIntervalUnsupportedReason = predictionIntervalUnsupportedReason,
    warnings = c(
      sprintf("Computed with stats::predict(type = 'response') for %s GLM.", familyName),
      if (isTRUE(requestsConfidenceInterval)) "Computed the GLM mean-response confidence interval on the link scale and transformed it to the response scale.",
      if (isTRUE(requestsPredictionInterval)) predictionIntervalUnsupportedReason,
      newDataInfo$warnings
    )
  )
  payload$glmFamily = familyName
  payload$glmLink = linkName
  payload$responseDescription = responseDescription
  payload
}

#' Compute a response-scale confidence interval for a GLM mean response
#'
#' @keywords internal
#' @noRd
computeGlmMeanResponseConfidenceInterval = function(model, newData, level = 0.95) {
  pred = stats::predict(model, newdata = newData, type = "link", se.fit = TRUE)
  fitLink = as.numeric(pred$fit)[1]
  seLink = as.numeric(pred$se.fit)[1]
  alpha = 1 - level
  critical = stats::qnorm(1 - alpha / 2)
  linkInv = model$family$linkinv

  list(
    fit = as.numeric(linkInv(fitLink)),
    lwr = as.numeric(linkInv(fitLink - critical * seLink)),
    upr = as.numeric(linkInv(fitLink + critical * seLink)),
    level = level,
    scale = "response"
  )
}
