#' Build a pure LLM score record from parsed scores
#'
#' Validates a parsed LLM scoring response and returns a score record
#' containing only judged fields, score summaries, and LLM scoring metadata.
#'
#' @param parsedScores Named list produced by `parseWmfmScoringJson()`.
#' @param modelName Optional character identifier for the scoring model.
#' @param rawResponse Raw character response returned by the LLM.
#'
#' @return Named list containing only scoring fields.
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
applyWmfmLlmScoresToRecord = function(
    parsedScores,
    modelName = NA_character_,
    rawResponse = NA_character_
) {
  parsedScores = validateWmfmParsedScores(parsedScores)

  scoreRecord = list(
    effectDirectionCorrect = parsedScores$effectDirectionCorrect,
    effectScaleAppropriate = parsedScores$effectScaleAppropriate,
    referenceGroupHandledCorrectly = parsedScores$referenceGroupHandledCorrectly,
    interactionCoverageAdequate = parsedScores$interactionCoverageAdequate,
    interactionSubstantiveCorrect = parsedScores$interactionSubstantiveCorrect,
    uncertaintyHandlingAppropriate = parsedScores$uncertaintyHandlingAppropriate,
    inferentialRegisterAppropriate = parsedScores$inferentialRegisterAppropriate,
    mainEffectCoverageAdequate = parsedScores$mainEffectCoverageAdequate,
    referenceGroupCoverageAdequate = parsedScores$referenceGroupCoverageAdequate,
    clarityAdequate = parsedScores$clarityAdequate,
    numericExpressionAdequate = parsedScores$numericExpressionAdequate,
    comparisonStructureClear = parsedScores$comparisonStructureClear,
    fatalFlawDetected = parsedScores$fatalFlawDetected,
    factualScore = parsedScores$factualScore,
    inferenceScore = parsedScores$inferenceScore,
    completenessScore = parsedScores$completenessScore,
    clarityScore = parsedScores$clarityScore,
    calibrationScore = parsedScores$calibrationScore,
    overallScore = parsedScores$overallScore,
    overallPass = parsedScores$overallPass,
    llmScored = TRUE,
    llmScoringModel = safeWmfmScalar(modelName),
    llmScoringRaw = safeWmfmScalar(rawResponse, naString = ""),
    llmScoringSummary = parsedScores$llmScoringSummary,
    llmFieldReasons = jsonlite::toJSON(
      parsedScores$fieldReasons,
      auto_unbox = TRUE,
      null = "null"
    )
  )

  scoreRecord
}
