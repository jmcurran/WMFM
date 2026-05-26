#' Detect and compute deterministic prediction payload from research questions
#'
#' @param model Fitted model object.
#' @param researchQuestion Character scalar research question.
#'
#' @return List payload with prediction metadata and deterministic prediction
#'   result when supported.
#' @keywords internal
#' @noRd
buildResearchQuestionPredictionPayload = function(model, researchQuestion) {
  question = trimws(as.character(researchQuestion %||% ""))
  if (!nzchar(question)) {
    return(NULL)
  }

  if (!isTRUE(isPredictionShapedResearchQuestion(question))) {
    return(NULL)
  }

  predictionResult = computeModelQuestionPrediction(
    model = model,
    followupQuestion = question,
    allowMissingPredictorCompletion = FALSE
  )

  list(
    category = if (isTRUE(predictionResult$predictionType == "individual_prediction_interval")) {
      "prediction_interval_request"
    } else {
      "prediction_request"
    },
    supported = TRUE,
    message = "Prediction-shaped research question detected for deterministic prediction pathway.",
    originalText = question,
    source = "research_question",
    predictionResult = predictionResult
  )
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
