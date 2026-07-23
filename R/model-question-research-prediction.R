#' Detect and compute deterministic prediction payload from research questions
#'
#' @param model Fitted model object.
#' @param researchQuestion Character scalar research question.
#'
#' @param includeQuestionRoute Logical scalar indicating whether the shared Stage
#'   47 route contract should be attached.
#'
#' @return List payload with prediction metadata and deterministic prediction
#'   result when supported.
#' @keywords internal
#' @noRd
buildResearchQuestionPredictionPayload = function(model, researchQuestion, includeQuestionRoute = TRUE) {
  question = trimws(as.character(researchQuestion %||% ""))
  if (!nzchar(question)) {
    return(NULL)
  }

  if (!isTRUE(isPredictionShapedResearchQuestion(question))) {
    return(NULL)
  }

  predictionInputValidation = validateLmPredictionInputs(
    model = model,
    followupQuestion = question,
    allowMissingPredictorCompletion = FALSE
  )

  if (!isTRUE(predictionInputValidation$ok)) {
    predictionResult = c(
      list(
        status = predictionInputValidation$status %||% "needs_input",
        reason = predictionInputValidation$reason %||% "missing_predictor_values",
        modelType = if (inherits(model, "glm")) "glm" else "lm",
        predictionType = "mean_response_prediction"
      ),
      predictionInputValidation[c(
        "suppliedPredictorValues",
        "requiredPredictors",
        "missingPredictors",
        "warnings"
      )]
    )
  } else {
    predictionResult = computeModelQuestionPrediction(
      model = model,
      followupQuestion = question,
      allowMissingPredictorCompletion = FALSE
    )
  }
  suppliedNames = names(predictionResult$suppliedPredictorValues %||% list())
  requiredPredictors = predictionResult$requiredPredictors %||% names(stats::model.frame(model))[-1]
  missingPredictors = setdiff(requiredPredictors, suppliedNames)
  if (identical(predictionResult$status, "ok") && length(missingPredictors) > 0) {
    predictionResult = list(
      status = "needs_input",
      reason = "missing_predictor_values",
      modelType = predictionResult$modelType %||% class(model)[[1]],
      predictionType = predictionResult$predictionType %||% "mean_response_prediction",
      suppliedPredictorValues = predictionResult$suppliedPredictorValues,
      requiredPredictors = requiredPredictors,
      missingPredictors = missingPredictors,
      warnings = paste0(
        "Missing fitted-model predictor values: ",
        paste(missingPredictors, collapse = ", "),
        "."
      )
    )
  }

  payload = list(
    category = if (isTRUE(predictionResult$predictionType == "individual_prediction_interval")) {
      "prediction_interval_request"
    } else {
      "prediction_request"
    },
    supported = TRUE,
    requiresDeterministicComputation = TRUE,
    message = "Prediction-shaped research question detected for deterministic prediction pathway.",
    originalText = question,
    normalizedText = tolower(question),
    source = "research_question",
    predictionResult = predictionResult
  )

  if (isTRUE(includeQuestionRoute)) {
    payload$questionRoute = routeExistingModelQuestionPayload(
      existingPayload = payload,
      source = "research_question"
    )
  }

  payload
}

#' @keywords internal
#' @noRd
isPredictionShapedResearchQuestion = function(researchQuestion) {
  text = tolower(trimws(as.character(researchQuestion %||% "")))
  if (!nzchar(text)) {
    return(FALSE)
  }

  hasPredictionVerb = grepl("\\b(predict|predicted|prediction|expected response|expected value|would .* get|what would .* be)\\b", text, perl = TRUE)
  hasAssociationOnly = grepl("\\b(relationship|relate|associated|association|increase as|decrease as|difference between|evidence of a difference|explain the relationship|how does .* change|as .*\\b(increase|increases|decrease|decreases|change|changes|vary|varies)\\b)\\b", text, perl = TRUE)

  hasPredictionVerb && !hasAssociationOnly
}
