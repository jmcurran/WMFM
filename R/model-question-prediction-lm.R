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

  followupQuestion = payload$originalText %||% ""
  payload$predictionResult = computeModelQuestionPrediction(
    model = model,
    followupQuestion = followupQuestion,
    allowMissingPredictorCompletion = TRUE
  )

  predictorNames = names(stats::model.frame(model))[-1]
  explicitlySuppliedPredictorValues = extractPredictionValuesForModel(
    model = model,
    followupQuestion = followupQuestion,
    allowSingleUnlabelledValue = FALSE
  )
  explicitlySuppliedPredictorValues[[".wmfm_unresolved_factor_predictors"]] = NULL
  missingPredictors = setdiff(
    predictorNames,
    names(explicitlySuppliedPredictorValues)
  )

  if (length(missingPredictors) > 0) {
    payload$predictionResult$status = "needs_input"
    payload$predictionResult$reason = "missing_predictor_values"
    payload$predictionResult$requiredPredictors = predictorNames
    payload$predictionResult$missingPredictors = missingPredictors
    payload$predictionResult$explicitlySuppliedPredictorValues =
      explicitlySuppliedPredictorValues
    payload$predictionResult$warnings = paste0(
      "Missing fitted-model predictor values: ",
      paste(missingPredictors, collapse = ", "),
      "."
    )
  }

  attachQuestionRouteToModelFollowupPayload(
    followupQuestion = followupQuestion,
    followupPayload = payload,
    model = model
  )
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

  predictionIntent = classifyPredictionQuestionIntent(tolower(followupQuestion))
  includeIndividual = isTRUE(inputValidation$requestsPredictionInterval) ||
    predictionIntent$target %in% c("individual_outcome", "ambiguous_personal")
  includeMean = isTRUE(inputValidation$requestsConfidenceInterval) ||
    predictionIntent$target %in% c("mean_response", "individual_outcome", "ambiguous_personal")
  predictionType = if (isTRUE(includeIndividual)) "individual_prediction_interval" else "mean_response_prediction"
  predictionValues = safelyComputeLmPredictions(
    model = model,
    newData = newDataInfo$newData,
    includeIndividual = includeIndividual,
    includeMean = includeMean
  )
  if (!isTRUE(predictionValues$ok)) {
    return(list(
      status = "needs_input",
      reason = "prediction_failed",
      modelType = "lm",
      predictionType = predictionType,
      suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
      resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
      completedPredictorValues = newDataInfo$completedPredictorValues,
      warnings = predictionValues$warning
    ))
  }

  predictionValues = backTransformLmPredictionValues(
    model = model,
    predictionValues = predictionValues
  )

  payload = formatModelQuestionPredictionPayload(
    modelType = "lm",
    predictionType = predictionType,
    responseScale = predictionValues$responseScale,
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    completedPredictorValues = newDataInfo$completedPredictorValues,
    fittedPrediction = predictionValues$fittedPrediction,
    confidenceInterval = predictionValues$confidenceInterval,
    predictionInterval = predictionValues$predictionInterval,
    predictionIntent = predictionIntent$target,
    predictionAmbiguity = predictionIntent$ambiguity,
    warnings = newDataInfo$warnings
  )

  payload$responseDescription = predictionValues$responseDescription
  payload$originalResponseVariable = predictionValues$originalResponseVariable
  payload$responseTransformation = predictionValues$responseTransformation
  payload$modelScalePrediction = predictionValues$modelScalePrediction
  payload
}

#' @keywords internal
#' @noRd
validateLmPredictionInputs = function(model, followupQuestion, allowMissingPredictorCompletion = TRUE) {
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  parsedPairs = extractPredictionValuesForModel(
    model = model,
    followupQuestion = followupQuestion,
    allowSingleUnlabelledValue = allowMissingPredictorCompletion
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
  # Strip sentence-ending punctuation/noise while preserving meaningful
  # variable-name punctuation such as parentheses in transformed predictors, plus signs,
  # slashes, and closing brackets that may appear at the end of a predictor.
  x = gsub("[[:space:]]*[.!?,;:]+$", "", x, perl = TRUE)
  trimws(x)
}

#' @keywords internal
#' @noRd
normalizePredictionPredictorName = function(x) {
  x = tolower(trimws(as.character(x %||% "")))
  x = gsub("[[:space:]]+", " ", x, perl = TRUE)
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
extractPredictionValuesForModel = function(model, followupQuestion, allowSingleUnlabelledValue = TRUE) {
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

    matchedValue = extractNaturalTransformedPredictionValue(
      predictor = predictor,
      text = text
    )
    if (is.null(matchedValue)) {
      matchedValue = extractNaturalNumericPredictionValue(
        predictor = predictor,
        text = text
      )
    }

    if (!is.null(matchedValue)) {
      parsedPairs[[predictor]] = matchedValue
    }
  }

  numericPredictors = predictorNames[vapply(predictorNames, function(predictor) {
    is.numeric(mf[[predictor]])
  }, logical(1))]
  missingNumericPredictors = setdiff(numericPredictors, names(parsedPairs))
  if (isTRUE(allowSingleUnlabelledValue) &&
      length(missingNumericPredictors) == 1L) {
    matchedValue = extractSingleNaturalPredictionNumber(text = text)
    if (!is.null(matchedValue)) {
      parsedPairs[[missingNumericPredictors[[1]]]] = matchedValue
    }
  }

  unresolvedFactors = character(0)
  for (predictor in predictorNames) {
    column = mf[[predictor]]
    modelLevels = character(0)
    if (is.factor(column)) {
      modelLevels = levels(column)
    } else if (!is.null(model$xlevels[[predictor]])) {
      modelLevels = model$xlevels[[predictor]]
    } else if (is.character(column)) {
      modelLevels = sort(unique(stats::na.omit(as.character(column))))
    }

    if (length(modelLevels) == 0) {
      next
    }

    explicitValue = parsedPairs[[predictor]]
    explicitMatched = character(0)

    if (!is.null(explicitValue) && nzchar(normalizePredictionText(explicitValue))) {
      explicitNorm = normalizePredictionText(explicitValue)
      explicitMatched = modelLevels[vapply(modelLevels, function(level) {
        identical(normalizePredictionText(level), explicitNorm)
      }, logical(1))]
    }

    namedMatched = matchNamedFactorLevelCandidate(
      predictor = predictor,
      modelLevels = modelLevels,
      text = text
    )
    if (length(namedMatched) == 1L) {
      parsedPairs[[predictor]] = namedMatched[[1]]
      next
    }

    textMatched = matchFactorLevelCandidates(text = text, modelLevels = modelLevels)
    semanticMatched = matchSemanticBinaryFactorLevel(
      predictor = predictor,
      modelLevels = modelLevels,
      text = text
    )
    aliasMatched = matchSemanticNamedFactorLevel(
      predictor = predictor,
      modelLevels = modelLevels,
      text = text
    )

    matched = unique(c(explicitMatched, textMatched, semanticMatched, aliasMatched))

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
  predictorNorm = normalizePredictionPredictorName(predictor)
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
  predictorPattern = paste0(
    "(?<![a-z0-9_.])",
    escapeRegexLiteral(predictorNorm),
    "(?![a-z0-9_.])"
  )
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
extractSingleNaturalPredictionNumber = function(text) {
  textNorm = normalizePredictionText(text)
  if (!nzchar(textNorm)) {
    return(NULL)
  }

  predictionCuePattern = paste(
    c(
      "\\b(score|scored|scores|mark|marks|test mark|test score)\\b",
      "\\bwith\\b",
      "\\bwho\\b",
      "\\bfor\\b"
    ),
    collapse = "|"
  )
  if (!grepl(predictionCuePattern, textNorm, perl = TRUE)) {
    return(NULL)
  }

  numberMatches = gregexpr("-?\\d+(?:\\.\\d+)?", textNorm, perl = TRUE)
  values = regmatches(textNorm, numberMatches)[[1]]
  if (!length(values) || identical(values, "-1")) {
    return(NULL)
  }

  numericValues = suppressWarnings(as.numeric(values))
  numericValues = numericValues[is.finite(numericValues)]
  if (length(numericValues) != 1L) {
    return(NULL)
  }

  as.character(numericValues[[1]])
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

  if (!nzchar(textNorm) || !nzchar(predictorNorm) || length(modelLevels) != 2) {
    return(character(0))
  }

  yesIndex = which(levelNorms %in% c("yes", "y", "true"))
  noIndex = which(levelNorms %in% c("no", "n", "false"))

  if (length(yesIndex) != 1 || length(noIndex) != 1) {
    return(character(0))
  }

  predictorPattern = escapeRegexLiteral(predictorNorm)
  positivePatterns = c(
    paste0("\\b", predictorPattern, "\\b.*\\b(yes|true)\\b"),
    paste0("\\b", predictorPattern, "(?:ed|s|ing)?\\s+(?:[a-z]+\\s+)?regularly\\b")
  )
  negativePatterns = c(
    paste0("\\b", predictorPattern, "\\b.*\\b(no|false)\\b"),
    paste0("\\b(?:do|does|did|would|will)?\\s*not\\s+", predictorPattern, "(?:ed|s|ing)?\\s+(?:[a-z]+\\s+)?regularly\\b")
  )

  hasPositive = any(vapply(positivePatterns, grepl, logical(1), x = textNorm, perl = TRUE))
  hasNegative = any(vapply(negativePatterns, grepl, logical(1), x = textNorm, perl = TRUE))

  if (isTRUE(hasPositive) && !isTRUE(hasNegative)) {
    return(modelLevels[[yesIndex]])
  }
  if (isTRUE(hasNegative) && !isTRUE(hasPositive)) {
    return(modelLevels[[noIndex]])
  }

  character(0)
}

#' @keywords internal
#' @noRd
matchNamedFactorLevelCandidate = function(predictor, modelLevels, text) {
  textNorm = normalizePredictionText(text)
  predictorNorm = normalizePredictionPredictorName(predictor)
  predictorAliases = predictorNorm
  if (identical(predictorNorm, "color")) {
    predictorAliases = c("color", "colour")
  } else if (identical(predictorNorm, "colour")) {
    predictorAliases = c("colour", "color")
  }

  matched = character(0)
  for (predictorAlias in predictorAliases) {
    for (level in modelLevels) {
      pattern = paste0(
        "\\b", escapeRegexLiteral(predictorAlias),
        "\\b\\s*(?:=|is|of|at)?\\s*",
        "\\b", escapeRegexLiteral(normalizePredictionText(level)), "\\b"
      )
      if (grepl(pattern, textNorm, perl = TRUE)) {
        matched = c(matched, level)
      }
    }
  }

  unique(matched)
}

#' @keywords internal
#' @noRd
extractNaturalTransformedPredictionValue = function(predictor, text) {
  predictorNorm = normalizePredictionPredictorName(predictor)
  matched = regmatches(
    predictorNorm,
    regexec("^log\\(([^()]+)\\)$", predictorNorm, perl = TRUE)
  )[[1]]
  if (length(matched) != 2L) {
    return(NULL)
  }

  sourcePredictor = matched[[2]]
  textNorm = normalizePredictionText(text)
  sourcePattern = escapeRegexLiteral(sourcePredictor)
  numberPattern = "(-?\\d+(?:\\.\\d+)?)"
  patterns = c(
    paste0(numberPattern, "\\s*", sourcePattern, "\\b"),
    paste0("\\b", sourcePattern, "\\b\\s*(?:=|is|of|at)?\\s*", numberPattern),
    paste0("\\bweigh(?:s|ing|ed)?\\s*", numberPattern, "\\s*", sourcePattern, "\\b")
  )

  for (pattern in patterns) {
    valueMatch = regmatches(textNorm, regexec(pattern, textNorm, perl = TRUE))[[1]]
    if (length(valueMatch) >= 2L) {
      sourceValue = suppressWarnings(as.numeric(valueMatch[[2]]))
      if (is.finite(sourceValue) && sourceValue > 0) {
        return(as.character(log(sourceValue)))
      }
    }
  }

  NULL
}

#' @keywords internal
#' @noRd
matchSemanticNamedFactorLevel = function(predictor, modelLevels, text) {
  textNorm = normalizePredictionText(text)
  predictorNorm = normalizePredictionText(predictor)
  levelNorms = vapply(modelLevels, normalizePredictionText, character(1))

  if (!nzchar(textNorm) || !length(modelLevels)) {
    return(character(0))
  }

  matched = character(0)

  geographicAliases = list(
    gr = "\\bgeorges?\\s+river\\b",
    `georges river` = "\\bgeorges?\\s+river\\b",
    ps1 = "\\b(?:port\\s+stephens\\s*(?:1|one)|first\\s+port\\s+stephens)\\b",
    ps2 = "\\b(?:port\\s+stephens\\s*(?:2|two)|second\\s+port\\s+stephens)\\b",
    sc = "\\bsouthern\\s+california\\b",
    `southern california` = "\\bsouthern\\s+california\\b",
    wa = "\\bwashington\\b",
    washington = "\\bwashington\\b"
  )

  for (levelIndex in seq_along(levelNorms)) {
    levelNorm = levelNorms[[levelIndex]]
    aliasPattern = geographicAliases[[levelNorm]]
    if (!is.null(aliasPattern) && grepl(aliasPattern, textNorm, perl = TRUE)) {
      matched = c(matched, modelLevels[[levelIndex]])
    }

    if (nzchar(levelNorm) && grepl(paste0("\\b", escapeRegexLiteral(levelNorm), "\\b"), textNorm, perl = TRUE)) {
      matched = c(matched, modelLevels[[levelIndex]])
    }
  }

  unique(matched)
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
  newData = data.frame(row.names = 1L)
  resolvedPredictorValues = list()
  completedPredictorValues = list()
  warnings = character(0)

  for (name in predictorNames) {
    value = suppliedPredictorValues[[name]]
    column = mf[[name]]
    valueWasSupplied = !is.null(value) && nzchar(trimws(as.character(value)))
    transformedPredictor = parseSimpleTransformedPredictor(name)

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

      if (is.list(transformedPredictor)) {
        sourceValue = invertSimplePredictionTransformation(
          value = numericValue,
          transformation = transformedPredictor$transformation
        )
        if (!is.finite(sourceValue)) {
          return(list(
            ok = FALSE,
            reason = "invalid_transformed_value",
            newData = newData,
            suppliedPredictorValues = suppliedPredictorValues,
            requiredPredictors = predictorNames,
            warnings = sprintf("WMFM could not recover a valid value for '%s' from predictor '%s'.", transformedPredictor$source, name)
          ))
        }
        newData[[transformedPredictor$source]] = sourceValue
        resolvedPredictorValues[[transformedPredictor$source]] = sourceValue
        completedPredictorValues[[transformedPredictor$source]] = sourceValue
      } else {
        newData[[name]] = numericValue
        resolvedPredictorValues[[name]] = numericValue
        completedPredictorValues[[name]] = numericValue
      }
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

#' Parse a simple transformed predictor term
#'
#' @param predictor Predictor term name from the fitted model frame.
#'
#' @return A list containing the transformation and source variable, or NULL.
#' @keywords internal
#' @noRd
parseSimpleTransformedPredictor = function(predictor) {
  predictorText = trimws(as.character(predictor %||% ""))
  patterns = list(
    log = "^log\\(([^()]+)\\)$",
    log1p = "^log1p\\(([^()]+)\\)$",
    sqrt = "^sqrt\\(([^()]+)\\)$"
  )

  for (transformation in names(patterns)) {
    matched = regmatches(
      predictorText,
      regexec(patterns[[transformation]], predictorText, perl = TRUE)
    )[[1]]
    if (length(matched) == 2L) {
      return(list(
        transformation = transformation,
        source = trimws(matched[[2]])
      ))
    }
  }

  NULL
}

#' Invert a supported predictor transformation
#'
#' @param value Numeric value on the transformed scale.
#' @param transformation Supported transformation name.
#'
#' @return Numeric value on the source-variable scale.
#' @keywords internal
#' @noRd
invertSimplePredictionTransformation = function(value, transformation) {
  value = suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NA_real_)
  }

  if (identical(transformation, "log")) {
    return(exp(value))
  }
  if (identical(transformation, "log1p")) {
    return(expm1(value))
  }
  if (identical(transformation, "sqrt")) {
    return(value^2)
  }

  NA_real_
}

#' Prepare a linear model for deterministic prediction
#'
#' Some app-fitted formulas retain a local formula environment after the fit
#' observer has completed. Transformed terms can then be evaluated against that
#' stale environment instead of resolving their source variables from
#' `newdata`. Bind the one-row prediction values into a child terms environment
#' so standard `predict.lm()` evaluation remains reliable.
#'
#' @param model Fitted ordinary linear model.
#' @param newData One-row prediction data frame.
#'
#' @return A prediction-safe copy of `model`.
#' @keywords internal
#' @noRd
prepareLmModelForPrediction = function(model, newData) {
  predictionModel = model
  termsObject = stats::terms(predictionModel)
  originalEnvironment = environment(termsObject)

  if (is.null(originalEnvironment)) {
    originalEnvironment = baseenv()
  }

  predictionEnvironment = new.env(parent = originalEnvironment)
  for (variableName in names(newData)) {
    assign(
      variableName,
      newData[[variableName]],
      envir = predictionEnvironment
    )
  }

  environment(termsObject) = predictionEnvironment
  predictionModel$terms = termsObject
  predictionModel
}


#' Back-transform linear-model prediction values to the original response scale
#'
#' @param model Fitted ordinary linear model.
#' @param predictionValues Prediction components returned by
#'   `safelyComputeLmPredictions()`.
#'
#' @return Prediction components with original-scale values when the fitted
#'   response uses a recognised invertible transformation.
#' @keywords internal
#' @noRd
backTransformLmPredictionValues = function(model, predictionValues) {
  transformation = getLmPredictionResponseTransformation(model = model)

  predictionValues$responseScale = "response"
  predictionValues$responseDescription = ""
  predictionValues$originalResponseVariable = names(stats::model.frame(model))[[1]]
  predictionValues$responseTransformation = transformation
  predictionValues$modelScalePrediction = NULL

  if (!isTRUE(transformation$available)) {
    return(predictionValues)
  }

  modelScalePrediction = list(
    responseScale = "transformed_response",
    responseVariable = transformation$transformedResponse,
    fittedPrediction = predictionValues$fittedPrediction,
    confidenceInterval = predictionValues$confidenceInterval,
    predictionInterval = predictionValues$predictionInterval
  )

  predictionValues$fittedPrediction = backTransformResponseValues(
    values = predictionValues$fittedPrediction,
    inverseType = transformation$inverseType,
    parameters = transformation$parameters
  )[[1]]
  predictionValues$confidenceInterval = backTransformLmPredictionInterval(
    interval = predictionValues$confidenceInterval,
    transformation = transformation
  )
  predictionValues$predictionInterval = backTransformLmPredictionInterval(
    interval = predictionValues$predictionInterval,
    transformation = transformation
  )
  predictionValues$responseScale = "original_response"
  predictionValues$responseDescription = transformation$originalResponse
  predictionValues$originalResponseVariable = transformation$originalResponse
  predictionValues$modelScalePrediction = modelScalePrediction
  predictionValues
}

#' Describe an invertible transformed response used by a linear model
#'
#' @param model Fitted ordinary linear model.
#'
#' @return A named transformation description.
#' @keywords internal
#' @noRd
getLmPredictionResponseTransformation = function(model) {
  responseExpression = stats::formula(model)[[2]]
  responseText = deparseOneLine(responseExpression)
  transformationInfo = inferTransformationType(responseExpression)
  inverseType = transformationInfo$inverseType %||% "unknown"
  sourceVariables = all.vars(responseExpression)

  if (
    length(sourceVariables) != 1L ||
      !isUsableResponseInverseType(inverseType = inverseType)
  ) {
    return(list(
      available = FALSE,
      transformedResponse = responseText,
      originalResponse = responseText,
      transformationType = transformationInfo$transformationType %||% "none",
      inverseType = inverseType,
      parameters = transformationInfo$parameters %||% list()
    ))
  }

  list(
    available = TRUE,
    transformedResponse = responseText,
    originalResponse = sourceVariables[[1]],
    transformationType = transformationInfo$transformationType,
    inverseType = inverseType,
    parameters = transformationInfo$parameters %||% list()
  )
}

#' Back-transform a linear-model prediction interval
#'
#' @param interval Prediction or confidence interval list, or `NULL`.
#' @param transformation Response transformation description.
#'
#' @return Back-transformed interval list, or `NULL`.
#' @keywords internal
#' @noRd
backTransformLmPredictionInterval = function(interval, transformation) {
  if (!is.list(interval)) {
    return(NULL)
  }

  values = backTransformResponseValues(
    values = c(interval$fit, interval$lwr, interval$upr),
    inverseType = transformation$inverseType,
    parameters = transformation$parameters
  )

  list(
    fit = values[[1]],
    lwr = values[[2]],
    upr = values[[3]],
    level = interval$level
  )
}

#' Compute linear-model predictions without allowing an app crash
#'
#' @param model Fitted ordinary linear model.
#' @param newData One-row prediction data frame.
#' @param includeIndividual Whether to calculate a prediction interval.
#' @param includeMean Whether to calculate a confidence interval for the mean.
#'
#' @return Prediction components with an ok flag and user-facing warning.
#' @keywords internal
#' @noRd
safelyComputeLmPredictions = function(model, newData, includeIndividual, includeMean) {
  predictionModel = prepareLmModelForPrediction(
    model = model,
    newData = newData
  )

  tryCatch(
    {
      fittedPrediction = as.numeric(stats::predict(predictionModel, newdata = newData))[1]
      predictionInterval = NULL
      confidenceInterval = NULL

      if (isTRUE(includeIndividual)) {
        piMat = stats::predict(predictionModel, newdata = newData, interval = "prediction")
        predictionInterval = list(
          fit = as.numeric(piMat[1, "fit"]),
          lwr = as.numeric(piMat[1, "lwr"]),
          upr = as.numeric(piMat[1, "upr"]),
          level = 0.95
        )
      }

      if (isTRUE(includeMean)) {
        ciMat = stats::predict(predictionModel, newdata = newData, interval = "confidence")
        confidenceInterval = list(
          fit = as.numeric(ciMat[1, "fit"]),
          lwr = as.numeric(ciMat[1, "lwr"]),
          upr = as.numeric(ciMat[1, "upr"]),
          level = 0.95
        )
      }

      list(
        ok = TRUE,
        fittedPrediction = fittedPrediction,
        confidenceInterval = confidenceInterval,
        predictionInterval = predictionInterval,
        warning = ""
      )
    },
    error = function(errorCondition) {
      list(
        ok = FALSE,
        fittedPrediction = NULL,
        confidenceInterval = NULL,
        predictionInterval = NULL,
        warning = paste0(
          "WMFM could not calculate this prediction from the supplied values. ",
          "Please check the predictor values or use explicit name = value wording."
        )
      )
    }
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
formatModelQuestionPredictionPayload = function(modelType, predictionType = "mean_response_prediction", responseScale = "response", suppliedPredictorValues, resolvedPredictorValues, completedPredictorValues = resolvedPredictorValues, fittedPrediction, confidenceInterval = NULL, predictionInterval = NULL, predictionIntent = "mean_response", predictionAmbiguity = "low", warnings = character(0)) {
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
    predictionIntent = predictionIntent,
    predictionAmbiguity = predictionAmbiguity,
    warnings = warnings
  )
}
