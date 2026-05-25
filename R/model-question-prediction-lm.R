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

  missingRequired = setdiff(predictorNames, suppliedNames)
  if (length(parsedPairs) == 0) {
    return(list(ok = FALSE, reason = "missing_predictor_values", suppliedPredictorValues = list(), requiredPredictors = predictorNames, warnings = "Provide predictor values in explicit `name = value` form."))
  }

  ambiguousFields = suppliedNames[grepl("^\\.__ambiguous__", suppliedNames, perl = TRUE)]
  if (length(ambiguousFields) > 0) {
    return(list(ok = FALSE, reason = "clarification_required", suppliedPredictorValues = parsedPairs, requiredPredictors = predictorNames, warnings = "Ambiguous predictor wording; please provide explicit values."))
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

  if (!("Test" %in% names(parsedPairs)) && ("Test" %in% predictorNames)) {
    if (grepl("\\bscore\\s+(\\d+(?:\\.\\d+)?)\\s+out\\s+of\\s+20\\b", text, perl = TRUE)) {
      matched = sub(".*\\bscore\\s+(\\d+(?:\\.\\d+)?)\\s+out\\s+of\\s+20\\b.*", "\\1", text, perl = TRUE)
      parsedPairs$Test = matched
    }
  }

  if (!("x" %in% names(parsedPairs)) && ("x" %in% predictorNames)) {
    if (grepl("\\bx\\s*=\\s*(-?\\d+(?:\\.\\d+)?)\\b", text, perl = TRUE)) {
      matched = sub(".*\\bx\\s*=\\s*(-?\\d+(?:\\.\\d+)?)\\b.*", "\\1", text, perl = TRUE)
      parsedPairs$x = matched
    } else if (grepl("\\baround\\s+\\d", text, perl = TRUE)) {
      parsedPairs$.__ambiguous__x = "vague_numeric_value"
    }
  }

  attendPredictorName = predictorNames[tolower(predictorNames) == "attend"][1]
  if (!is.na(attendPredictorName) && nzchar(attendPredictorName) &&
      !(attendPredictorName %in% names(parsedPairs))) {
    attendLevels = levels(mf[[attendPredictorName]])
    if (grepl("\\b(attend|attendance)\\b", text, perl = TRUE) &&
        grepl("\\b(regular|regularly|yes)\\b", text, perl = TRUE) &&
        !grepl("\\b(not|does not|don't|do not)\\b", text, perl = TRUE)) {
      regularLevel = attendLevels[grepl("regular", tolower(attendLevels), perl = TRUE)][1]
      if (is.na(regularLevel) || !nzchar(regularLevel)) {
        regularLevel = attendLevels[1]
      }
      parsedPairs[[attendPredictorName]] = regularLevel
    } else if (grepl("\\b(attend|attendance)\\b", text, perl = TRUE) &&
      grepl("\\b(not|does not|don't|do not)\\b", text, perl = TRUE) &&
      !grepl("\\b(regular|regularly|yes)\\b", text, perl = TRUE)) {
      notLevel = attendLevels[grepl("^not$|^no$|none|irregular", tolower(attendLevels), perl = TRUE)][1]
      if (!is.na(notLevel) && nzchar(notLevel)) {
        parsedPairs[[attendPredictorName]] = notLevel
      } else {
        parsedPairs[[paste0(".__ambiguous__", attendPredictorName)]] = "unresolved_factor_level"
      }
    } else if (grepl("\\b(attend|attendance)\\b", text, perl = TRUE) &&
      grepl("\\b(regular|regularly|yes)\\b", text, perl = TRUE) &&
      grepl("\\b(not|does not|don't|do not)\\b", text, perl = TRUE)) {
      parsedPairs[[paste0(".__ambiguous__", attendPredictorName)]] = "ambiguous_factor_level"
    }
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
