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

  linkPrediction = stats::predict(model, newdata = newDataInfo$newData, type = "link", se.fit = TRUE)
  linkFit = as.numeric(linkPrediction$fit)[1]
  linkSe = as.numeric(linkPrediction$se.fit)[1]
  fittedPrediction = as.numeric(model$family$linkinv(linkFit))[1]
  confidenceInterval = NULL
  if (is.finite(linkFit) && is.finite(linkSe)) {
    linkLwr = linkFit - stats::qnorm(0.975) * linkSe
    linkUpr = linkFit + stats::qnorm(0.975) * linkSe
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
    predictionInterval = NULL,
    warnings = c(
      sprintf("Computed with stats::predict(type = 'response') for %s GLM.", familyName),
      newDataInfo$warnings
    )
  )
  payload$glmFamily = familyName
  payload$glmLink = linkName
  payload$responseDescription = responseDescription
  payload$predictionIntervalUnsupportedReason = "GLM follow-up prediction intervals for a future observation are not currently supported; WMFM reports a confidence interval for the fitted mean response instead."
  payload
}
