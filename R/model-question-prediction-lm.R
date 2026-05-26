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
        status = inputValidation$status %||% ifelse(identical(inputValidation$reason, "clarification_required"), "clarification_required", "needs_input"),
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
  unresolvedFactors = parsedPairs[[".wmfm_unresolved_factor_predictors"]] %||% character(0)
  parsedPairs[[".wmfm_unresolved_factor_predictors"]] = NULL
  suppliedNames = names(parsedPairs)

  missingRequired = setdiff(predictorNames, suppliedNames)
  if (length(parsedPairs) == 0) {
    return(list(ok = FALSE, reason = "missing_predictor_values", suppliedPredictorValues = list(), requiredPredictors = predictorNames, warnings = "Provide predictor values in explicit `name = value` form."))
  }

  if (length(unresolvedFactors) > 0) {
    return(list(
      ok = FALSE,
      status = "clarification_required",
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


normalizePredictionText = function(x) {
  x = tolower(trimws(as.character(x %||% "")))
  gsub("[[:space:]]+", " ", x, perl = TRUE)
}

isSimpleWordLevel = function(levelText) {
  grepl("^[a-z0-9_]+$", levelText, perl = TRUE)
}

#' @keywords internal
#' @noRd
extractPredictionValuesForModel = function(model, followupQuestion) {
  parsedPairs = extractPredictionAssignmentPairs(followupQuestion = followupQuestion)
  text = normalizePredictionText(followupQuestion)
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]

  if (length(parsedPairs) > 0) {
    canonicalized = list()
    canonicalNames = character(0)
    for (key in names(parsedPairs)) {
      keyNorm = normalizePredictionText(key)
      matchedPredictor = predictorNames[vapply(predictorNames, function(pn) {
        identical(normalizePredictionText(pn), keyNorm)
      }, logical(1))]
      resolvedName = if (length(matchedPredictor) == 1) matchedPredictor[[1]] else key
      canonicalized[[resolvedName]] = parsedPairs[[key]]
      canonicalNames = c(canonicalNames, resolvedName)
    }
    parsedPairs = canonicalized
  }

  if (!("Test" %in% names(parsedPairs)) &&
      ("Test" %in% predictorNames) &&
      grepl("\\b(\\d+(?:\\.\\d+)?)\\s*out\\s*of\\s*20\\b", text, perl = TRUE)) {
    matched = sub(".*\\b(\\d+(?:\\.\\d+)?)\\s*out\\s*of\\s*20\\b.*", "\\1", text, perl = TRUE)
    parsedPairs$Test = matched
  }

  unresolvedFactors = character(0)
  for (predictor in predictorNames) {
    column = mf[[predictor]]
    if (!is.factor(column)) next

    modelLevels = levels(column)
    explicitValue = parsedPairs[[predictor]]
    explicitMatched = character(0)

    if (!is.null(explicitValue) && nzchar(normalizePredictionText(explicitValue))) {
      explicitNorm = normalizePredictionText(explicitValue)
      explicitMatched = modelLevels[vapply(modelLevels, function(level) {
        identical(normalizePredictionText(level), explicitNorm)
      }, logical(1))]
    }

    textMatched = matchFactorLevelCandidates(text = text, modelLevels = modelLevels)

    matched = unique(c(explicitMatched, textMatched))

    if (length(matched) == 1) {
      parsedPairs[[predictor]] = matched[[1]]
    } else if (length(matched) > 1) {
      unresolvedFactors = unique(c(unresolvedFactors, predictor))
      parsedPairs[[predictor]] = NULL
    }
  }

  if (length(unresolvedFactors) > 0) {
    parsedPairs[[".wmfm_unresolved_factor_predictors"]] = unresolvedFactors
  }

  parsedPairs
}

#' @keywords internal
#' @noRd
containsStandaloneLevel = function(text, levelText) {
  text = normalizePredictionText(text)
  levelText = normalizePredictionText(levelText)
  if (!nzchar(text) || !nzchar(levelText)) {
    return(FALSE)
  }

  matchPos = gregexpr(levelText, text, fixed = TRUE)[[1]]
  hasSimpleTokenVariant = FALSE
  if (isSimpleWordLevel(levelText)) {
    tokens = unlist(strsplit(text, "[^a-z0-9_]+", perl = TRUE), use.names = FALSE)
    tokens = tokens[nzchar(tokens)]
    if (any(tokens == levelText)) {
      hasSimpleTokenVariant = TRUE
    } else if (any(startsWith(tokens, levelText) & (nchar(tokens) - nchar(levelText) <= 2L))) {
      # Conservative morphological variant support:
      # allow "regularly" -> "regular" only when level itself is a simple token.
      hasSimpleTokenVariant = TRUE
    }
  }

  if (length(matchPos) == 1 && identical(matchPos[[1]], -1L)) {
    return(hasSimpleTokenVariant)
  }

  levelLength = nchar(levelText, type = "chars")
  textLength = nchar(text, type = "chars")

  isBoundary = function(ch) {
    if (!nzchar(ch)) return(TRUE)
    !grepl("[[:alnum:]_]", ch, perl = TRUE)
  }

  for (startPos in matchPos) {
    beforeChar = if (startPos <= 1) "" else substr(text, startPos - 1, startPos - 1)
    afterPos = startPos + levelLength
    afterChar = if (afterPos > textLength) "" else substr(text, afterPos, afterPos)
    if (isBoundary(beforeChar) && isBoundary(afterChar)) {
      return(TRUE)
    }
  }

  hasSimpleTokenVariant
}

#' @keywords internal
#' @noRd
matchFactorLevelCandidates = function(text, modelLevels) {
  textNorm = normalizePredictionText(text)
  levelsNorm = vapply(modelLevels, normalizePredictionText, character(1))
  isPunctuated = !vapply(levelsNorm, isSimpleWordLevel, logical(1))

  # Pass 1: direct, boundary-aware literal matching across all levels.
  boundaryMatches = modelLevels[vapply(levelsNorm, function(levelNorm) {
    containsStandaloneLevel(text = textNorm, levelText = levelNorm)
  }, logical(1))]
  if (length(boundaryMatches) > 0) {
    return(boundaryMatches)
  }

  # Pass 2: for punctuated levels (A+B, yes/no, group (1), x{2}), allow literal
  # substring matching without regex to avoid metacharacter bugs.
  punctuatedMask = rep(FALSE, length(modelLevels))
  punctuatedIdx = which(isPunctuated)
  if (length(punctuatedIdx) > 0) {
    punctuatedMask[punctuatedIdx] = vapply(levelsNorm[punctuatedIdx], function(levelNorm) {
      grepl(levelNorm, textNorm, fixed = TRUE)
    }, logical(1))
  }
  punctuatedMatches = modelLevels[punctuatedMask]
  if (length(punctuatedMatches) > 0) {
    return(punctuatedMatches)
  }

  character(0)
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
  m = gregexpr("([A-Za-z][A-Za-z0-9_.]*)\\s*=\\s*([^,;]+)", text, perl = TRUE)
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
      val = sub("\\s+(?:and|with|for|when|where|what|which)\\b.*$", "", val, perl = TRUE)
      val = trimws(val)
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
