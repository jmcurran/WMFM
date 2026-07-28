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
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return(list(
      category = "research_profile_comparison",
      status = "unsupported",
      reason = "ordinary_lm_required",
      warnings = "Stage 49.4 profile comparisons currently support ordinary linear models."
    ))
  }

  question = trimws(as.character(researchQuestion %||% ""))
  pieces = strsplit(question, "(?i)\\b(?:versus|vs\\.?|compared with|compared to)\\b", perl = TRUE)[[1]]
  pieces = trimws(pieces)
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
    comparisonDirection = "right_minus_left"
  )
}

#' @keywords internal
#' @noRd
buildDeterministicResearchQuestionComparisonAnswer = function(payload, model) {
  if (!is.list(payload) || !identical(payload$status, "ok")) {
    return("")
  }

  responseName = names(stats::model.frame(model))[[1]]
  paste0(
    "For ", formatFollowupPredictorSettings(payload$leftProfile),
    ", the estimated average ", responseName, " is ",
    formatFollowupPredictionNumber(payload$leftExpectedResponse), ". For ",
    formatFollowupPredictorSettings(payload$rightProfile),
    ", it is ", formatFollowupPredictionNumber(payload$rightExpectedResponse),
    ". The second profile is estimated to differ from the first by ",
    formatFollowupPredictionNumber(payload$difference),
    ", with a 95% confidence interval from ",
    formatFollowupPredictionNumber(payload$confidenceInterval$lwr), " to ",
    formatFollowupPredictionNumber(payload$confidenceInterval$upr),
    ". This interval describes uncertainty in the difference between expected responses; it is not an individual prediction interval."
  )
}
