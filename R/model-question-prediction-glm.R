#' Compute deterministic generalised-linear-model follow-up prediction payload
#'
#' @param model Fitted model object.
#' @param followupQuestion Character scalar bounded follow-up question.
#'
#' @return Named list prediction payload.
#' @keywords internal
#' @noRd
computeGlmModelQuestionPrediction = function(model, followupQuestion, allowMissingPredictorCompletion = FALSE) {
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
  if (!(familyName %in% c("binomial", "poisson"))) {
    return(list(
      status = "unsupported",
      reason = "unsupported_glm_family",
      modelType = "glm",
      predictionType = "mean_response_prediction",
      responseScale = "response",
      warnings = "This stage supports deterministic GLM mean predictions only for binomial logistic and Poisson models."
    ))
  }

  lowerText = tolower(as.character(followupQuestion %||% ""))
  requestsPredictionInterval = grepl("\\bprediction intervals?\\b", lowerText, perl = TRUE)
  requestsConfidenceInterval = grepl("\\bconfidence intervals?\\b", lowerText, perl = TRUE)

  # GLM deterministic follow-up predictions require explicit fitted-model
  # predictor values. Unlike the lm student follow-up pathway, this route does
  # not complete omitted covariates, because GLM response-scale predictions are
  # easy to overstate when the conditioning values are not fully specified.
  inputValidation = validateLmPredictionInputs(
    model = model,
    followupQuestion = followupQuestion,
    allowMissingPredictorCompletion = FALSE
  )

  if (isTRUE(requestsPredictionInterval) || isTRUE(requestsConfidenceInterval)) {
    requestedType = if (isTRUE(requestsPredictionInterval)) {
      "individual_prediction_interval"
    } else {
      "confidence_interval_for_mean_response"
    }
    return(c(
      list(
        status = "unsupported",
        reason = "unsupported_glm_interval_request",
        modelType = "glm",
        glmFamily = familyName,
        predictionType = requestedType,
        responseScale = "response"
      ),
      inputValidation[c("suppliedPredictorValues", "requiredPredictors")],
      list(
        warnings = paste(
          "Deterministic GLM interval support is not implemented in Stage 23.11.",
          "Use mean-response prediction on the response scale, or provide follow-up interval support in a later stage."
        )
      )
    ))
  }

  if (!isTRUE(inputValidation$ok)) {
    return(c(
      list(
        status = "needs_input",
        reason = inputValidation$reason,
        modelType = "glm",
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
        predictionType = "mean_response_prediction",
        responseScale = "response"
      ),
      newDataInfo[c("suppliedPredictorValues", "requiredPredictors", "warnings")]
    ))
  }

  fittedPrediction = as.numeric(stats::predict(model, newdata = newDataInfo$newData, type = "response"))[1]
  formatModelQuestionPredictionPayload(
    modelType = "glm",
    predictionType = "mean_response_prediction",
    responseScale = "response",
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    completedPredictorValues = newDataInfo$completedPredictorValues,
    fittedPrediction = fittedPrediction,
    warnings = c(
      sprintf("Computed with stats::predict(type = 'response') for %s GLM.", familyName),
      newDataInfo$warnings
    )
  )
}
