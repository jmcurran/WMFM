#' Build deterministic expected-response and profile-comparison payloads
#'
#' @param model Fitted model object.
#' @param researchQuestion Character scalar research question.
#' @param archetype Resolved question archetype.
#'
#' @return A deterministic payload, or `NULL` when the archetype is not handled.
#' @keywords internal
#' @noRd
buildResearchQuestionAnswerPayload = function(model, researchQuestion, archetype) {
  if (identical(archetype, "expected_response")) {
    return(buildResearchQuestionPredictionPayload(
      model = model,
      researchQuestion = researchQuestion,
      includeQuestionRoute = FALSE
    ))
  }

  if (identical(archetype, "compare_groups_or_profiles")) {
    return(computeResearchQuestionProfileComparison(model, researchQuestion))
  }

  NULL
}

#' @keywords internal
#' @noRd
computeResearchQuestionProfileComparison = function(model, researchQuestion) {
  if (!inherits(model, "lm")) {
    return(list(
      category = "research_profile_comparison",
      status = "unsupported",
      reason = "supported_regression_model_required",
      warnings = "Stage 49.5 profile comparisons support ordinary linear, logistic, and Poisson regression models."
    ))
  }

  question = trimws(as.character(researchQuestion %||% ""))
  pieces = strsplit(question, "(?i)\\b(?:versus|vs\\.?|compared with|compared to)\\b", perl = TRUE)[[1]]
  pieces = trimws(pieces)

  if (length(pieces) != 2L) {
    naturalProfiles = buildNaturalBinaryComparisonProfiles(
      model = model,
      researchQuestion = question
    )
    if (is.list(naturalProfiles)) {
      pieces = c(naturalProfiles$leftQuestion, naturalProfiles$rightQuestion)
    }
  }
  if (length(pieces) != 2L || any(!nzchar(pieces))) {
    return(list(
      category = "research_profile_comparison",
      status = "needs_input",
      reason = "two_profiles_required",
      missingPredictors = "comparison_profiles",
      warnings = paste(
        "State two complete profiles separated by 'versus', for example:",
        "Compare the expected response for X = 2 versus X = 4."
      )
    ))
  }

  left = computeModelQuestionPrediction(model, pieces[[1]], allowMissingPredictorCompletion = FALSE)
  right = computeModelQuestionPrediction(model, pieces[[2]], allowMissingPredictorCompletion = FALSE)
  if (!identical(left$status, "ok") || !identical(right$status, "ok")) {
    missing = unique(c(left$missingPredictors %||% character(0), right$missingPredictors %||% character(0)))
    return(list(
      category = "research_profile_comparison",
      status = "needs_input",
      reason = "incomplete_comparison_profiles",
      missingPredictors = missing,
      left = left,
      right = right,
      warnings = paste(c(left$warnings, right$warnings), collapse = " ")
    ))
  }

  if (inherits(model, "glm")) {
    return(computeGlmResearchQuestionProfileComparison(model, left, right))
  }

  computeLmResearchQuestionProfileComparison(model, left, right)
}


#' @keywords internal
#' @noRd
buildNaturalBinaryComparisonProfiles = function(model, researchQuestion) {
  mf = stats::model.frame(model)
  predictorNames = names(mf)[-1]
  text = normalizePredictionText(researchQuestion)

  factorPredictors = predictorNames[vapply(predictorNames, function(name) {
    is.factor(mf[[name]]) || length(model$xlevels[[name]] %||% character(0)) == 2L
  }, logical(1))]
  numericPredictors = predictorNames[vapply(predictorNames, function(name) {
    is.numeric(mf[[name]])
  }, logical(1))]

  if (length(factorPredictors) != 1L || length(numericPredictors) != 1L) {
    return(NULL)
  }

  factorName = factorPredictors[[1]]
  numericName = numericPredictors[[1]]
  levels = if (is.factor(mf[[factorName]])) levels(mf[[factorName]]) else model$xlevels[[factorName]]
  levelNorms = tolower(levels)
  yesIndex = which(levelNorms %in% c("yes", "y", "true"))
  noIndex = which(levelNorms %in% c("no", "n", "false"))

  hasPositiveNegativeContrast = grepl("\\battend(?:ing|ed|s)?\\b.*\\bnon[- ]?attend(?:ing|ed|s)?\\b|\\bnon[- ]?attend(?:ing|ed|s)?\\b.*\\battend(?:ing|ed|s)?\\b", text, perl = TRUE)
  if (length(yesIndex) != 1L || length(noIndex) != 1L || !isTRUE(hasPositiveNegativeContrast)) {
    return(NULL)
  }

  sharedValue = extractSingleNaturalPredictionNumber(text)
  if (is.null(sharedValue)) {
    return(NULL)
  }

  list(
    leftQuestion = paste0(factorName, " = ", levels[[yesIndex]], ", ", numericName, " = ", sharedValue),
    rightQuestion = paste0(factorName, " = ", levels[[noIndex]], ", ", numericName, " = ", sharedValue)
  )
}

#' @keywords internal
#' @noRd
computeLmResearchQuestionProfileComparison = function(model, left, right) {
  leftData = buildLmPredictionNewData(model, left$resolvedPredictorValues)$newData
  rightData = buildLmPredictionNewData(model, right$resolvedPredictorValues)$newData
  termsObject = stats::delete.response(stats::terms(model))
  leftMatrix = stats::model.matrix(termsObject, leftData, contrasts.arg = model$contrasts, xlev = model$xlevels)
  rightMatrix = stats::model.matrix(termsObject, rightData, contrasts.arg = model$contrasts, xlev = model$xlevels)
  differenceVector = as.numeric(rightMatrix[1, ] - leftMatrix[1, ])
  estimate = as.numeric(right$fittedPrediction - left$fittedPrediction)
  standardError = sqrt(as.numeric(t(differenceVector) %*% stats::vcov(model) %*% differenceVector))
  criticalValue = stats::qt(0.975, df = stats::df.residual(model))

  list(
    category = "research_profile_comparison",
    status = "ok",
    reason = "ok",
    modelFamily = "gaussian",
    responseDescription = "mean_response",
    leftProfile = left$resolvedPredictorValues,
    rightProfile = right$resolvedPredictorValues,
    leftExpectedResponse = left$fittedPrediction,
    rightExpectedResponse = right$fittedPrediction,
    difference = estimate,
    confidenceInterval = list(
      lwr = estimate - criticalValue * standardError,
      upr = estimate + criticalValue * standardError,
      level = 0.95
    ),
    intervalType = "confidence_interval_for_expected_response_difference",
    comparisonDirection = "right_minus_left",
    intervalMethod = "linear_model_contrast"
  )
}

#' @keywords internal
#' @noRd
computeGlmResearchQuestionProfileComparison = function(model, left, right) {
  familyName = tolower(model$family$family %||% "")
  if (!familyName %in% c("binomial", "poisson")) {
    return(list(
      category = "research_profile_comparison",
      status = "unsupported",
      reason = "unsupported_glm_family",
      modelFamily = familyName,
      warnings = "Stage 49.5 GLM profile comparisons support binomial logistic and Poisson models."
    ))
  }

  leftData = buildLmPredictionNewData(model, left$resolvedPredictorValues)$newData
  rightData = buildLmPredictionNewData(model, right$resolvedPredictorValues)$newData
  termsObject = stats::delete.response(stats::terms(model))
  leftMatrix = stats::model.matrix(termsObject, leftData, contrasts.arg = model$contrasts, xlev = model$xlevels)
  rightMatrix = stats::model.matrix(termsObject, rightData, contrasts.arg = model$contrasts, xlev = model$xlevels)

  coefficients = stats::coef(model)
  leftEta = as.numeric(leftMatrix[1, ] %*% coefficients)
  rightEta = as.numeric(rightMatrix[1, ] %*% coefficients)
  leftMean = as.numeric(model$family$linkinv(leftEta))
  rightMean = as.numeric(model$family$linkinv(rightEta))
  leftDerivative = as.numeric(model$family$mu.eta(leftEta))
  rightDerivative = as.numeric(model$family$mu.eta(rightEta))
  gradient = rightDerivative * as.numeric(rightMatrix[1, ]) - leftDerivative * as.numeric(leftMatrix[1, ])
  standardError = sqrt(as.numeric(t(gradient) %*% stats::vcov(model) %*% gradient))
  estimate = rightMean - leftMean
  criticalValue = stats::qnorm(0.975)
  lower = estimate - criticalValue * standardError
  upper = estimate + criticalValue * standardError
  if (identical(familyName, "binomial")) {
    lower = max(-1, lower)
    upper = min(1, upper)
  }

  list(
    category = "research_profile_comparison",
    status = "ok",
    reason = "ok",
    modelFamily = familyName,
    responseDescription = if (identical(familyName, "binomial")) "probability" else "expected_count",
    leftProfile = left$resolvedPredictorValues,
    rightProfile = right$resolvedPredictorValues,
    leftExpectedResponse = leftMean,
    rightExpectedResponse = rightMean,
    difference = estimate,
    confidenceInterval = list(
      lwr = lower,
      upr = upper,
      level = 0.95
    ),
    intervalType = "confidence_interval_for_expected_response_difference",
    comparisonDirection = "right_minus_left",
    intervalMethod = "response_scale_delta_method"
  )
}

#' Format deterministic comparison results for presentation
#'
#' @param value Numeric scalar.
#'
#' @return Character scalar with two decimal places.
#' @keywords internal
#' @noRd
formatResearchQuestionComparisonNumber = function(value) {
  value = suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value)) {
    return(NA_character_)
  }

  formatC(value, format = "f", digits = 2)
}

#' @keywords internal
#' @noRd
buildDeterministicResearchQuestionComparisonAnswer = function(payload, model) {
  if (!is.list(payload) || !identical(payload$status, "ok")) {
    return("")
  }

  responseName = names(stats::model.frame(model))[[1]]
  quantityText = switch(
    payload$responseDescription %||% "mean_response",
    probability = paste0("predicted probability of ", responseName),
    expected_count = paste0("expected count for ", responseName),
    paste0("estimated average ", responseName)
  )
  limitationText = switch(
    payload$responseDescription %||% "mean_response",
    probability = "These are fitted probabilities, not guaranteed binary outcomes for individuals.",
    expected_count = "These are expected counts, not exact future counts.",
    "This interval describes uncertainty in the difference between expected responses; it is not an individual prediction interval."
  )

  difference = as.numeric(payload$difference)
  differenceText = if (is.finite(difference) && difference < 0) {
    paste0(
      "The second profile is estimated to have ", responseName, " ",
      formatResearchQuestionComparisonNumber(abs(difference)),
      " units lower than the first"
    )
  } else if (is.finite(difference) && difference > 0) {
    paste0(
      "The second profile is estimated to have ", responseName, " ",
      formatResearchQuestionComparisonNumber(difference),
      " units higher than the first"
    )
  } else {
    "The two profiles have the same estimated response"
  }

  paste0(
    "For ", formatFollowupPredictorSettings(payload$leftProfile),
    ", the ", quantityText, " is ",
    formatResearchQuestionComparisonNumber(payload$leftExpectedResponse), ". For ",
    formatFollowupPredictorSettings(payload$rightProfile),
    ", it is ", formatResearchQuestionComparisonNumber(payload$rightExpectedResponse),
    ". ", differenceText,
    ", with a 95% confidence interval for the second-minus-first difference from ",
    formatResearchQuestionComparisonNumber(payload$confidenceInterval$lwr), " to ",
    formatResearchQuestionComparisonNumber(payload$confidenceInterval$upr),
    ". ", limitationText
  )
}
