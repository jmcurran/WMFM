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
  if (!is.list(payload) || !(identical(payload$category, "prediction_request") || identical(payload$category, "prediction_interval_request"))) {
    return(payload)
  }

  payload$predictionResult = computeModelQuestionPrediction(
    model = model,
    followupQuestion = payload$originalText %||% ""
  )
  payload
}

#' @keywords internal
#' @noRd
computeModelQuestionPrediction = function(model, followupQuestion) {
  if (inherits(model, "glm")) {
    return(computeGlmModelQuestionPrediction(model = model, followupQuestion = followupQuestion))
  }
  computeLmModelQuestionPrediction(model = model, followupQuestion = followupQuestion)
}

#' @keywords internal
#' @noRd
computeLmModelQuestionPrediction = function(model, followupQuestion) {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_type",
      reasonDetail = "stage23.7_supports_only_ordinary_lm_prediction_intervals",
      modelType = class(model)[[1]],
      predictionType = "mean_response_prediction",
      warnings = "This pathway currently supports ordinary linear-model prediction follow-ups only (mean-response and individual prediction interval)."
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

  if (!isTRUE(newDataInfo$ok)) {
    return(c(
      list(
        status = "needs_input",
        reason = newDataInfo$reason,
        modelType = "lm",
        predictionType = "mean_response_prediction"
      ),
      newDataInfo[c("suppliedPredictorValues", "requiredPredictors", "warnings")]
    ))
  }

  predictionType = if (isTRUE(inputValidation$requestsPredictionInterval)) "individual_prediction_interval" else "mean_response_prediction"
  predFit = as.numeric(stats::predict(model, newdata = newDataInfo$newData))[1]
  confidenceInterval = NULL
  predictionInterval = NULL

  if (isTRUE(inputValidation$requestsPredictionInterval)) {
    piMat = stats::predict(model, newdata = newDataInfo$newData, interval = "prediction")
    predictionInterval = list(
      fit = as.numeric(piMat[1, "fit"]),
      lwr = as.numeric(piMat[1, "lwr"]),
      upr = as.numeric(piMat[1, "upr"]),
      level = 0.95
    )
  } else if (isTRUE(inputValidation$requestsConfidenceInterval)) {
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
    predictionType = predictionType,
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    fittedPrediction = predFit,
    confidenceInterval = confidenceInterval,
    predictionInterval = predictionInterval,
    warnings = newDataInfo$warnings
  )
}

#' @keywords internal
#' @noRd
validateLmPredictionInputs = function(model, followupQuestion) {
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  parsedPairs = extractPredictionValuesForModel(
    model = model,
    followupQuestion = followupQuestion
  )
  suppliedNames = names(parsedPairs)
  unresolvedFactors = parsedPairs[[".wmfm_unresolved_factor_predictors"]] %||% character(0)
  parsedPairs[[".wmfm_unresolved_factor_predictors"]] = NULL

  missingRequired = setdiff(predictorNames, suppliedNames)
  if (length(parsedPairs) == 0) {
    return(list(ok = FALSE, reason = "missing_predictor_values", suppliedPredictorValues = list(), requiredPredictors = predictorNames, warnings = "Provide predictor values in explicit `name = value` form."))
  }

  if (length(unresolvedFactors) > 0) {
    return(list(
      ok = FALSE,
      reason = "clarification_required",
      suppliedPredictorValues = parsedPairs,
      requiredPredictors = predictorNames,
      warnings = paste0("Ambiguous factor wording for: ", paste(unresolvedFactors, collapse = ", "), ". Please provide explicit values in `name = value` form.")
    ))
  }

  unknownNames = setdiff(suppliedNames, predictorNames)
  if (length(unknownNames) > 0) {
    return(list(ok = FALSE, reason = "unsupported_request_type", suppliedPredictorValues = parsedPairs, requiredPredictors = predictorNames, warnings = paste0("Unknown predictors: ", paste(unknownNames, collapse = ", "))))
  }

  lowerText = tolower(followupQuestion)
  requestsPredictionInterval = grepl("\\bprediction intervals?\\b", lowerText, perl = TRUE)
  requestsConfidenceInterval = grepl("\\bconfidence interval\\b", lowerText, perl = TRUE)
  list(ok = length(missingRequired) == 0, reason = ifelse(length(missingRequired) == 0, "ok", "missing_predictor_values"), suppliedPredictorValues = parsedPairs, requiredPredictors = predictorNames, requestsPredictionInterval = requestsPredictionInterval, requestsConfidenceInterval = requestsConfidenceInterval, warnings = ifelse(length(missingRequired) == 0, "", paste0("Missing predictor values: ", paste(missingRequired, collapse = ", "))))
}

#' @keywords internal
#' @noRd
extractPredictionValuesForModel = function(model, followupQuestion) {
  parsedPairs = extractPredictionAssignmentPairs(followupQuestion = followupQuestion)
  text = tolower(trimws(as.character(followupQuestion %||% "")))
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]

  if (!("Test" %in% names(parsedPairs)) &&
      ("Test" %in% predictorNames) &&
      grepl("\\b(\\d+(?:\\.\\d+)?)\\s*out\\s*of\\s*20\\b", text, perl = TRUE)) {
    matched = sub(".*\\b(\\d+(?:\\.\\d+)?)\\s*out\\s*of\\s*20\\b.*", "\\1", text, perl = TRUE)
    parsedPairs$Test = matched
  }

  unresolvedFactors = character(0)
  for (predictor in predictorNames) {
    if (predictor %in% names(parsedPairs)) next
    column = mf[[predictor]]
    if (!is.factor(column)) next

    levelsLower = tolower(levels(column))
    matched = levels(column)[vapply(levelsLower, function(levelText) {
      pattern = paste0("\\b", gsub("([.|()\\[\\]{}+*?^$\\\\])", "\\\\\\1", levelText), "\\b")
      grepl(pattern, text, perl = TRUE)
    }, logical(1))]

    if (length(matched) == 1) {
      parsedPairs[[predictor]] = matched[[1]]
    } else if (length(matched) > 1) {
      unresolvedFactors = c(unresolvedFactors, predictor)
    }
  }

  if (length(unresolvedFactors) > 0) {
    parsedPairs[[".wmfm_unresolved_factor_predictors"]] = unresolvedFactors
  }

  parsedPairs
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
        return(list(
          ok = FALSE,
          reason = "invalid_factor_level",
          newData = newData,
          suppliedPredictorValues = suppliedPredictorValues,
          requiredPredictors = predictorNames,
          warnings = sprintf("Unsupported factor level '%s' for predictor '%s'.", value, name)
        ))
      }
      newData[[name]] = factor(value, levels = lvl)
      resolvedPredictorValues[[name]] = value
    } else if (is.numeric(column)) {
      numericValue = suppressWarnings(as.numeric(value))
      if (!is.finite(numericValue)) {
        return(list(
          ok = FALSE,
          reason = "invalid_numeric_value",
          newData = newData,
          suppliedPredictorValues = suppliedPredictorValues,
          requiredPredictors = predictorNames,
          warnings = sprintf("Predictor '%s' requires a numeric value.", name)
        ))
      }
      newData[[name]] = numericValue
      resolvedPredictorValues[[name]] = numericValue
    } else {
      return(list(
        ok = FALSE,
        reason = "unsupported_predictor_type",
        newData = newData,
        suppliedPredictorValues = suppliedPredictorValues,
        requiredPredictors = predictorNames,
        warnings = sprintf("Predictor '%s' has unsupported type for this deterministic prediction pathway.", name)
      ))
    }
  }

  list(ok = TRUE, newData = newData, suppliedPredictorValues = suppliedPredictorValues, requiredPredictors = predictorNames, resolvedPredictorValues = resolvedPredictorValues, warnings = warnings)
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
formatModelQuestionPredictionPayload = function(modelType, predictionType = "mean_response_prediction", responseScale = "response", suppliedPredictorValues, resolvedPredictorValues, completedPredictorValues = resolvedPredictorValues, fittedPrediction, confidenceInterval = NULL, predictionInterval = NULL, warnings = character(0)) {
  list(
    status = "ok",
    modelType = modelType,
    predictionType = predictionType,
    responseScale = responseScale,
    suppliedPredictorValues = suppliedPredictorValues,
    resolvedPredictorValues = resolvedPredictorValues,
    completedPredictorValues = completedPredictorValues,
    fittedPrediction = fittedPrediction,
    confidenceInterval = confidenceInterval,
    predictionInterval = predictionInterval,
    warnings = warnings
  )
}
