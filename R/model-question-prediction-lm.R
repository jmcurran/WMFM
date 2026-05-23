#' Compute deterministic linear-model follow-up prediction payload
#'
#' @param model Fitted model object.
#' @param followupPayload List returned by \code{classifyModelFollowupQuestion()}.
#'
#' @return Updated payload list. Adds \code{predictionResult} for prediction
#'   requests.
#' @keywords internal
#' @noRd
enrichFollowupPayloadWithLmPrediction = function(model, followupPayload) {
  payload = followupPayload
  if (!is.list(payload) || !identical(payload$category, "prediction_request")) {
    return(payload)
  }

  payload$predictionResult = computeLmModelQuestionPrediction(
    model = model,
    followupQuestion = payload$originalText %||% ""
  )
  payload
}

#' @keywords internal
#' @noRd
computeLmModelQuestionPrediction = function(model, followupQuestion) {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "stage23.6_supports_only_ordinary_lm",
      modelType = class(model)[[1]],
      predictionType = "mean_response_prediction",
      warnings = "WMFM Stage 23.6 only supports ordinary linear-model mean-response predictions."
    ))
  }

  inputValidation = validateLmPredictionInputs(model = model, followupQuestion = followupQuestion)
  if (!isTRUE(inputValidation$ok)) {
    return(c(
      list(
        status = "needs_input",
        reason = inputValidation$reason,
        modelType = "lm",
        predictionType = "mean_response_prediction"
      ),
      inputValidation[c("suppliedPredictorValues", "requiredPredictors", "warnings")]
    ))
  }

  newDataInfo = buildLmPredictionNewData(
    model = model,
    suppliedPredictorValues = inputValidation$suppliedPredictorValues
  )

  predFit = as.numeric(stats::predict(model, newdata = newDataInfo$newData))[1]
  confidenceInterval = NULL
  if (isTRUE(inputValidation$requestsConfidenceInterval)) {
    ciMat = stats::predict(model, newdata = newDataInfo$newData, interval = "confidence")
    confidenceInterval = list(
      fit = as.numeric(ciMat[1, "fit"]),
      lwr = as.numeric(ciMat[1, "lwr"]),
      upr = as.numeric(ciMat[1, "upr"]),
      level = 0.95
    )
  }

  formatModelQuestionPredictionPayload(
    modelType = "lm",
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    fittedPrediction = predFit,
    confidenceInterval = confidenceInterval,
    warnings = newDataInfo$warnings
  )
}

#' @keywords internal
#' @noRd
validateLmPredictionInputs = function(model, followupQuestion) {
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  parsedPairs = extractPredictionAssignmentPairs(followupQuestion = followupQuestion)
  suppliedNames = names(parsedPairs)

  missingRequired = setdiff(predictorNames, suppliedNames)
  if (length(parsedPairs) == 0) {
    return(list(ok = FALSE, reason = "no_structured_predictor_values", suppliedPredictorValues = list(), requiredPredictors = predictorNames, warnings = "Provide predictor values in explicit `name = value` form."))
  }

  unknownNames = setdiff(suppliedNames, predictorNames)
  if (length(unknownNames) > 0) {
    return(list(ok = FALSE, reason = "unknown_predictor_names", suppliedPredictorValues = parsedPairs, requiredPredictors = predictorNames, warnings = paste0("Unknown predictors: ", paste(unknownNames, collapse = ", "))))
  }

  requestsConfidenceInterval = grepl("\\bconfidence interval\\b", tolower(followupQuestion), perl = TRUE)
  list(ok = length(missingRequired) == 0, reason = ifelse(length(missingRequired) == 0, "ok", "missing_required_predictor_values"), suppliedPredictorValues = parsedPairs, requiredPredictors = predictorNames, requestsConfidenceInterval = requestsConfidenceInterval, warnings = ifelse(length(missingRequired) == 0, "", paste0("Missing predictor values: ", paste(missingRequired, collapse = ", "))))
}

#' @keywords internal
#' @noRd
buildLmPredictionNewData = function(model, suppliedPredictorValues) {
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  newData = mf[1, predictorNames, drop = FALSE]
  resolvedPredictorValues = list()
  warnings = character(0)

  for (name in predictorNames) {
    value = suppliedPredictorValues[[name]]
    column = mf[[name]]
    if (is.factor(column)) {
      lvl = levels(column)
      if (!(value %in% lvl)) {
        stop(sprintf("Unsupported factor level '%s' for predictor '%s'.", value, name), call. = FALSE)
      }
      newData[[name]] = factor(value, levels = lvl)
      resolvedPredictorValues[[name]] = value
    } else if (is.numeric(column)) {
      numericValue = suppressWarnings(as.numeric(value))
      if (!is.finite(numericValue)) {
        stop(sprintf("Predictor '%s' requires a numeric value.", name), call. = FALSE)
      }
      newData[[name]] = numericValue
      resolvedPredictorValues[[name]] = numericValue
    } else {
      warnings = c(warnings, sprintf("Predictor '%s' has unsupported type for Stage 23.6.", name))
    }
  }

  list(newData = newData, suppliedPredictorValues = suppliedPredictorValues, resolvedPredictorValues = resolvedPredictorValues, warnings = warnings)
}

#' @keywords internal
#' @noRd
extractPredictionAssignmentPairs = function(followupQuestion) {
  text = as.character(followupQuestion %||% "")
  m = gregexpr("([A-Za-z][A-Za-z0-9_.]*)\\s*=\\s*([A-Za-z0-9_.-]+)", text, perl = TRUE)
  pieces = regmatches(text, m)[[1]]
  out = list()
  if (length(pieces) == 0) {
    return(out)
  }
  for (piece in pieces) {
    kv = strsplit(piece, "=", fixed = TRUE)[[1]]
    if (length(kv) == 2) {
      key = trimws(kv[[1]])
      val = trimws(kv[[2]])
      out[[key]] = val
    }
  }
  out
}

#' @keywords internal
#' @noRd
formatModelQuestionPredictionPayload = function(modelType, suppliedPredictorValues, resolvedPredictorValues, fittedPrediction, confidenceInterval = NULL, warnings = character(0)) {
  list(
    status = "ok",
    modelType = modelType,
    predictionType = "mean_response_prediction",
    suppliedPredictorValues = suppliedPredictorValues,
    resolvedPredictorValues = resolvedPredictorValues,
    fittedPrediction = fittedPrediction,
    confidenceInterval = confidenceInterval,
    warnings = warnings
  )
}
