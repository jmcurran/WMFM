#' Classify comparable-observation follow-up questions
#'
#' @param normalizedText Lower-case normalized question text.
#'
#' @return Named list with `matched` and `reasonCode`.
#' @keywords internal
#' @noRd
classifyComparableObservationQuestion = function(normalizedText) {
  text = trimws(as.character(normalizedText %||% ""))
  result = list(matched = FALSE, reasonCode = NULL)
  if (!nzchar(text)) {
    return(result)
  }

  comparisonPattern = paste(
    c(
      "\\bgood deal\\b",
      "\\bbargain\\b",
      "\\bcomparable (observation|observations|case|cases|examples?)\\b",
      "\\bsimilar (observation|observations|case|cases|examples?|diamonds?|students?|houses?|patients?)\\b",
      "\\bcompare (this|the) (observation|case|diamond|student|house|patient)\\b",
      "\\bwhat do similar\\b",
      "\\bhow does (this|the) .+ compare\\b"
    ),
    collapse = "|"
  )
  caseConditionPattern = paste(
    c(
      "\\bfor\\b.+\\b[[:alnum:]][[:alnum:]_.-]*\\b",
      "\\bwith\\b.+\\b[[:alnum:]][[:alnum:]_.-]*\\b",
      "\\bwhen\\b.+\\b[[:alnum:]][[:alnum:]_.-]*\\b",
      "\\bat\\b.+\\b[[:digit:]]+(?:\\.[[:digit:]]+)?\\b",
      "\\b[[:digit:]]+(?:\\.[[:digit:]]+)?[- ]?(carat|caret|unit|year|point)\\b",
      "\\b(this|the) (observation|case|diamond|student|house|patient)\\b"
    ),
    collapse = "|"
  )

  if (grepl(comparisonPattern, text, perl = TRUE) &&
      grepl(caseConditionPattern, text, perl = TRUE)) {
    result$matched = TRUE
    result$reasonCode = "comparable_observations_requested"
  }
  result
}

#' Add comparable-observation computation to a follow-up payload
#'
#' @param model Fitted model object.
#' @param followupPayload Follow-up classification payload.
#' @param neighbourCount Maximum number of comparable observations retained.
#'
#' @return Updated payload with `comparableObservationResult` when relevant.
#' @keywords internal
#' @noRd
enrichFollowupPayloadWithComparableObservations = function(model, followupPayload, neighbourCount = 10L) {
  payload = followupPayload
  if (!is.list(payload) || !identical(payload$category, "comparable_observation_request")) {
    return(payload)
  }
  payload$comparableObservationResult = computeComparableObservationResult(
    model = model,
    followupQuestion = payload$originalText %||% "",
    neighbourCount = neighbourCount
  )
  payload$requiresDeterministicComputation = TRUE
  payload
}

#' @keywords internal
#' @noRd
computeComparableObservationResult = function(model, followupQuestion, neighbourCount = 10L) {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(
      status = "unsupported",
      reason = "ordinary_lm_required",
      warnings = "Comparable-observation matching currently supports ordinary linear models only."
    ))
  }

  inputValidation = validateLmPredictionInputs(
    model = model,
    followupQuestion = followupQuestion,
    allowMissingPredictorCompletion = TRUE
  )
  if (!isTRUE(inputValidation$ok)) {
    return(c(
      list(status = inputValidation$status %||% "needs_input", reason = inputValidation$reason),
      inputValidation[c("suppliedPredictorValues", "requiredPredictors", "warnings")]
    ))
  }

  newDataInfo = buildLmPredictionNewData(
    model = model,
    suppliedPredictorValues = inputValidation$suppliedPredictorValues
  )
  if (!isTRUE(newDataInfo$ok)) {
    return(c(list(status = "needs_input", reason = newDataInfo$reason), newDataInfo[c("warnings")]))
  }

  modelFrame = stats::model.frame(model)
  predictorFrame = modelFrame[-1]
  targetFrame = tryCatch(
    stats::model.frame(
      stats::delete.response(stats::terms(model)),
      data = newDataInfo$newData,
      xlev = model$xlevels,
      na.action = stats::na.pass
    ),
    error = function(e) NULL
  )
  if (is.null(targetFrame) || !identical(names(targetFrame), names(predictorFrame))) {
    return(list(
      status = "unsupported",
      reason = "target_predictors_could_not_be_constructed",
      warnings = "WMFM could not construct comparable predictor values on the fitted-model scale."
    ))
  }

  contributionMatrix = matrix(NA_real_, nrow = nrow(predictorFrame), ncol = ncol(predictorFrame))
  colnames(contributionMatrix) = names(predictorFrame)
  for (index in seq_along(predictorFrame)) {
    column = predictorFrame[[index]]
    target = targetFrame[[index]][[1]]
    if (is.numeric(column)) {
      scaleRange = diff(range(column, na.rm = TRUE))
      contributionMatrix[, index] = if (is.finite(scaleRange) && scaleRange > 0) {
        abs(column - as.numeric(target)) / scaleRange
      } else {
        0
      }
    } else {
      contributionMatrix[, index] = as.numeric(as.character(column) != as.character(target))
    }
  }

  availableCount = rowSums(!is.na(contributionMatrix))
  distance = rowSums(contributionMatrix, na.rm = TRUE) / availableCount
  distance[availableCount == 0] = NA_real_

  neighbourCount = suppressWarnings(as.integer(neighbourCount[[1]] %||% 10L))
  if (is.na(neighbourCount) || neighbourCount < 1L) {
    neighbourCount = 10L
  }
  sourceRows = suppressWarnings(as.integer(row.names(modelFrame)))
  if (anyNA(sourceRows)) {
    sourceRows = seq_len(nrow(modelFrame))
  }
  orderIndex = order(distance, sourceRows, na.last = NA)
  keep = head(orderIndex, min(neighbourCount, length(orderIndex)))
  response = stats::model.response(modelFrame)
  observationLabels = row.names(modelFrame)
  sequentialLabels = identical(observationLabels, as.character(seq_along(observationLabels)))
  if (sequentialLabels) {
    observationLabels = paste("Row", sourceRows)
  }

  responseTransformation = getLmPredictionResponseTransformation(model = model)
  responseValues = as.numeric(response[keep])
  responseName = names(modelFrame)[[1]]
  responseScale = "fitted_model_response_scale"

  if (isTRUE(responseTransformation$available)) {
    responseValues = backTransformResponseValues(
      values = responseValues,
      inverseType = responseTransformation$inverseType,
      parameters = responseTransformation$parameters
    )
    responseName = responseTransformation$originalResponse
    responseScale = "original_response"
  }

  comparable = data.frame(
    rank = seq_along(keep),
    observation = observationLabels[keep],
    row = sourceRows[keep],
    distance = as.numeric(distance[keep]),
    response = as.numeric(responseValues),
    stringsAsFactors = FALSE
  )

  list(
    status = "ok",
    reason = "ok",
    modelType = "lm",
    responseName = responseName,
    responseScale = responseScale,
    responseTransformation = responseTransformation,
    distanceMethod = "mean range-standardised numeric distance plus factor mismatch",
    suppliedPredictorValues = newDataInfo$suppliedPredictorValues,
    resolvedPredictorValues = newDataInfo$resolvedPredictorValues,
    completedPredictorValues = newDataInfo$completedPredictorValues,
    predictorsUsed = names(predictorFrame),
    neighbourCount = nrow(comparable),
    totalFittedObservations = nrow(modelFrame),
    observations = comparable,
    warnings = newDataInfo$warnings,
    limitations = c(
      "Similarity is defined only by predictors in the fitted model.",
      "Numeric predictors are scaled by their observed ranges and factor mismatches contribute zero or one.",
      "The selected observations do not by themselves establish that a case is a bargain, unusual, or causally different."
    )
  )
}


#' Format comparable-observation rows for deterministic prompt context
#'
#' @param observations Comparable-observation data frame.
#'
#' @return Character scalar containing one line per observation.
#' @keywords internal
#' @noRd
formatComparableObservationRows = function(observations) {
  if (!is.data.frame(observations) || !nrow(observations)) {
    return("No comparable observations supplied.")
  }

  rows = vapply(seq_len(nrow(observations)), function(index) {
    row = observations[index, , drop = FALSE]
    sprintf(
      "Rank %s: %s; source row %s; distance %s; observed response %s",
      row$rank[[1]],
      row$observation[[1]],
      row$row[[1]],
      signif(as.numeric(row$distance[[1]]), 6),
      signif(as.numeric(row$response[[1]]), 6)
    )
  }, character(1))

  paste(rows, collapse = "\n")
}
