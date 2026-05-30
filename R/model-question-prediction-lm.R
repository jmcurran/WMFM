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
  if (!is.list(payload)) {
    return(payload)
  }

  if (identical(payload$category, "unit_change_request")) {
    return(enrichFollowupPayloadWithUnitChange(
      model = model,
      followupPayload = payload
    ))
  }

  if (!(identical(payload$category, "prediction_request") || identical(payload$category, "prediction_interval_request"))) {
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
computeModelQuestionPrediction = function(model, followupQuestion, allowMissingPredictorCompletion = TRUE) {
  if (inherits(model, "glm")) {
    return(computeGlmModelQuestionPrediction(
      model = model,
      followupQuestion = followupQuestion,
      allowMissingPredictorCompletion = allowMissingPredictorCompletion
    ))
  }
  computeLmModelQuestionPrediction(
    model = model,
    followupQuestion = followupQuestion,
    allowMissingPredictorCompletion = allowMissingPredictorCompletion
  )
}

#' @keywords internal
#' @noRd
computeLmModelQuestionPrediction = function(model, followupQuestion, allowMissingPredictorCompletion = TRUE) {
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
    completedPredictorValues = newDataInfo$completedPredictorValues,
    fittedPrediction = predFit,
    confidenceInterval = confidenceInterval,
    predictionInterval = predictionInterval,
    warnings = newDataInfo$warnings
  )
}

#' @keywords internal
#' @noRd
validateLmPredictionInputs = function(model, followupQuestion, allowMissingPredictorCompletion = TRUE) {
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

  if (length(missingRequired) > 0 && !isTRUE(allowMissingPredictorCompletion)) {
    return(list(
      ok = FALSE,
      status = "needs_input",
      reason = "missing_predictor_values",
      suppliedPredictorValues = parsedPairs,
      requiredPredictors = predictorNames,
      missingPredictors = missingRequired,
      warnings = paste0(
        "Missing fitted-model predictor values: ",
        paste(missingRequired, collapse = ", "),
        "."
      )
    ))
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
  requestsPredictionInterval = grepl("\\bprediction intervals?\\b", lowerText, perl = TRUE) ||
    isIndividualLmPredictionRequest(lowerText)
  requestsConfidenceInterval = grepl("\\bconfidence interval\\b", lowerText, perl = TRUE) &&
    !isTRUE(requestsPredictionInterval)

  list(
    ok = TRUE,
    reason = "ok",
    suppliedPredictorValues = parsedPairs,
    requiredPredictors = predictorNames,
    missingPredictors = missingRequired,
    requestsPredictionInterval = requestsPredictionInterval,
    requestsConfidenceInterval = requestsConfidenceInterval,
    warnings = ifelse(
      length(missingRequired) == 0,
      "",
      paste0(
        "Omitted fitted-model predictor values will be completed deterministically: ",
        paste(missingRequired, collapse = ", "),
        "."
      )
    )
  )
}

#' Detect individual-outcome prediction wording for lm follow-up questions
#'
#' @param lowerText Lower-case follow-up question text.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
isIndividualLmPredictionRequest = function(lowerText) {
  text = as.character(lowerText %||% "")
  hasPredictionWording = grepl("\\b(predict|predicted|prediction|would|will)\\b", text, perl = TRUE)
  hasIndividualWording = grepl("\\b(i|me|my|student|person|individual|case|observation)\\b", text, perl = TRUE)
  hasOutcomeQuestion = grepl("\\bwhat\\b.*\\b(mark|score|value|outcome|response)\\b", text, perl = TRUE)

  isTRUE(hasPredictionWording && (hasIndividualWording || hasOutcomeQuestion))
}


normalizePredictionText = function(x) {
  x = tolower(trimws(as.character(x %||% "")))
  x = gsub("[[:space:]]+", " ", x, perl = TRUE)
  # Strip trailing punctuation/noise while preserving legitimate leading and
  # internal factor punctuation such as + and / (e.g., +, /A, A+B, yes/no).
  x = gsub("[^[:alnum:]_+/-]+$", "", x, perl = TRUE)
  trimws(x)
}

isSimpleWordLevel = function(levelText) {
  grepl("^[a-z0-9_]+$", levelText, perl = TRUE)
}

stripTrailingAssignmentPunctuation = function(valueText) {
  valueText = trimws(as.character(valueText %||% ""))
  if (!nzchar(valueText)) {
    return(valueText)
  }
  trimws(sub("[?.,;:]+$", "", valueText, perl = TRUE))
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

  for (predictor in predictorNames) {
    if (predictor %in% names(parsedPairs)) {
      next
    }

    column = mf[[predictor]]
    if (!is.numeric(column)) {
      next
    }

    matchedValue = extractNaturalNumericPredictionValue(
      predictor = predictor,
      text = text
    )

    if (!is.null(matchedValue)) {
      parsedPairs[[predictor]] = matchedValue
    }
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
    semanticMatched = matchSemanticBinaryFactorLevel(
      predictor = predictor,
      modelLevels = modelLevels,
      text = text
    )

    matched = unique(c(explicitMatched, textMatched, semanticMatched))

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
extractNaturalNumericPredictionValue = function(predictor, text) {
  predictorNorm = normalizePredictionText(predictor)
  textNorm = normalizePredictionText(text)
  if (!nzchar(predictorNorm) || !nzchar(textNorm)) {
    return(NULL)
  }

  extractFirstCapture = function(pattern) {
    match = regmatches(textNorm, regexec(pattern, textNorm, perl = TRUE))[[1]]
    if (length(match) < 2) {
      return(NULL)
    }
    match[[2]]
  }

  numberPattern = "(-?\\d+(?:\\.\\d+)?)"
  predictorPattern = paste0("\\b", predictorNorm, "\\b")
  nearbyAfterPattern = paste0(
    predictorPattern,
    "(?:\\s+(?:is|of|at|mark|score|value|equal(?:s)?|=|would be))*\\s+",
    numberPattern
  )
  nearbyBeforePattern = paste0(
    numberPattern,
    "(?:\\s+(?:on|for|in|as|at|out of \\d+))*\\s+",
    predictorPattern
  )

  matchedValue = extractFirstCapture(nearbyAfterPattern)
  if (!is.null(matchedValue)) {
    return(matchedValue)
  }

  matchedValue = extractFirstCapture(nearbyBeforePattern)
  if (!is.null(matchedValue)) {
    return(matchedValue)
  }

  NULL
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
matchSemanticBinaryFactorLevel = function(predictor, modelLevels, text) {
  textNorm = normalizePredictionText(text)
  predictorNorm = normalizePredictionText(predictor)
  levelNorms = vapply(modelLevels, normalizePredictionText, character(1))

  if (!nzchar(textNorm) || length(modelLevels) != 2) {
    return(character(0))
  }

  yesIndex = which(levelNorms %in% c("yes", "y", "true"))
  noIndex = which(levelNorms %in% c("no", "n", "false"))

  if (length(yesIndex) != 1 || length(noIndex) != 1) {
    return(character(0))
  }

  if (predictorNorm %in% c("attend", "attendance", "attended")) {
    if (grepl("\\b(do not|does not|did not|don'?t|not)\\s+attend\\b", textNorm, perl = TRUE) ||
        grepl("\\bwithout\\s+(regular\\s+)?attend", textNorm, perl = TRUE)) {
      return(modelLevels[[noIndex]])
    }

    hasPositiveAttendance = grepl("\\battend(s|ed|ing)?\\b", textNorm, perl = TRUE) &&
      grepl("\\b(regular|regularly|yes)\\b", textNorm, perl = TRUE)

    if (isTRUE(hasPositiveAttendance)) {
      return(modelLevels[[yesIndex]])
    }
  }

  character(0)
}

#' @keywords internal
#' @noRd
matchFactorLevelCandidates = function(text, modelLevels) {
  matchFactorLevelsInPredictionText(levels = modelLevels, text = text)
}

#' @keywords internal
#' @noRd
matchFactorLevelsInPredictionText = function(levels, text) {
  textNorm = normalizePredictionText(text)
  if (!nzchar(textNorm) || length(levels) == 0) {
    return(character(0))
  }

  matchedLevels = character(0)
  for (level in levels) {
    levelNorm = normalizePredictionText(level)
    if (!nzchar(levelNorm)) next
    if (containsStandaloneLevel(text = textNorm, levelText = levelNorm)) {
      matchedLevels = c(matchedLevels, level)
    }
  }

  unique(matchedLevels)
}

#' @keywords internal
#' @noRd
matchFactorLevelInFollowupText = function(levels, followupText) {
  matchFactorLevelsInPredictionText(levels = levels, text = followupText)
}


#' @keywords internal
#' @noRd
buildLmPredictionNewData = function(model, suppliedPredictorValues) {
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  newData = mf[1, predictorNames, drop = FALSE]
  resolvedPredictorValues = list()
  completedPredictorValues = list()
  warnings = character(0)

  for (name in predictorNames) {
    value = suppliedPredictorValues[[name]]
    column = mf[[name]]
    valueWasSupplied = !is.null(value) && nzchar(trimws(as.character(value)))

    categoricalLevels = character(0)
    if (is.factor(column)) {
      categoricalLevels = levels(column)
    } else if (!is.null(model$xlevels[[name]])) {
      categoricalLevels = model$xlevels[[name]]
    } else if (is.character(column)) {
      categoricalLevels = sort(unique(stats::na.omit(as.character(column))))
    }

    if (length(categoricalLevels) > 0) {
      lvl = categoricalLevels
      if (!isTRUE(valueWasSupplied)) {
        value = lvl[[1]]
        warnings = c(
          warnings,
          sprintf(
            "Predictor '%s' was not supplied; WMFM used the reference level '%s' to complete the fitted-model prediction.",
            name,
            value
          )
        )
      }
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
      completedPredictorValues[[name]] = value
    } else if (is.numeric(column)) {
      if (!isTRUE(valueWasSupplied)) {
        numericValue = mean(column, na.rm = TRUE)
        warnings = c(
          warnings,
          sprintf(
            "Predictor '%s' was not supplied; WMFM used the sample mean %.6g to complete the fitted-model prediction.",
            name,
            numericValue
          )
        )
      } else {
        numericValue = suppressWarnings(as.numeric(value))
      }
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
      completedPredictorValues[[name]] = numericValue
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

  list(
    ok = TRUE,
    newData = newData,
    suppliedPredictorValues = suppliedPredictorValues,
    requiredPredictors = predictorNames,
    resolvedPredictorValues = resolvedPredictorValues,
    completedPredictorValues = completedPredictorValues,
    warnings = warnings
  )
}

#' @keywords internal
#' @noRd
extractPredictionAssignmentPairs = function(followupQuestion) {
  text = as.character(followupQuestion %||% "")
  out = list()
  if (!nzchar(trimws(text))) {
    return(out)
  }

  keyPattern = "[A-Za-z][A-Za-z0-9_.]*"
  assignmentPattern = paste0(
    "(", keyPattern, ")\\s*=\\s*",
    "(.*?)",
    "(?=\\s+(?:and|with|for|when|where)\\s+", keyPattern, "\\s*=|[,;]|$)"
  )

  m = gregexpr(assignmentPattern, text, perl = TRUE)
  pieces = regmatches(text, m)[[1]]
  if (length(pieces) == 0 || identical(pieces, character(0))) {
    return(out)
  }

  for (piece in pieces) {
    kv = strsplit(piece, "=", fixed = TRUE)[[1]]
    if (length(kv) < 2) {
      next
    }

    key = trimws(kv[[1]])
    val = trimws(paste(kv[-1], collapse = "="))
    val = sub("\\s+(?:and|with|for|when|where|what|which)\\b.*$", "", val, perl = TRUE)
    val = trimws(val)
    val = stripTrailingAssignmentPunctuation(val)
    if (nzchar(key) && nzchar(val)) {
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
