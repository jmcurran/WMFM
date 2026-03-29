#' Apply parsed LLM scores to a WMFM run record
#'
#' Validates a parsed LLM scoring response and writes the scored fields back into
#' a run record produced by `buildWmfmRunRecord()`.
#'
#' @param runRecord Named list produced by `buildWmfmRunRecord()`.
#' @param parsedScores Named list produced by `parseWmfmScoringJson()`.
#' @param modelName Optional character identifier for the scoring model.
#' @param rawResponse Raw character response returned by the LLM.
#'
#' @return Updated run record with scored fields populated.
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
applyWmfmLlmScoresToRecord = function(
    runRecord,
    parsedScores,
    modelName = NA_character_,
    rawResponse = NA_character_
) {

  if (!is.list(runRecord) || is.null(names(runRecord))) {
    stop("`runRecord` must be a named list.", call. = FALSE)
  }

  parsedScores = validateWmfmParsedScores(parsedScores)

  rubricFields = c(
    "effectDirectionCorrect",
    "effectScaleAppropriate",
    "referenceGroupHandledCorrectly",
    "interactionCoverageAdequate",
    "interactionSubstantiveCorrect",
    "uncertaintyHandlingAppropriate",
    "inferentialRegisterAppropriate",
    "mainEffectCoverageAdequate",
    "referenceGroupCoverageAdequate",
    "clarityAdequate",
    "numericExpressionAdequate",
    "comparisonStructureClear"
  )

  numericFields = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore",
    "overallScore"
  )

  for (field in rubricFields) {
    runRecord[[field]] = parsedScores[[field]]
  }

  for (field in numericFields) {
    runRecord[[field]] = parsedScores[[field]]
  }

  runRecord$fatalFlawDetected = parsedScores$fatalFlawDetected
  runRecord$overallPass = parsedScores$overallPass

  runRecord$llmScored = TRUE
  runRecord$llmScoringModel = safeWmfmScalar(modelName)
  runRecord$llmScoringRaw = safeWmfmScalar(rawResponse, naString = "")
  runRecord$llmScoringSummary = parsedScores$llmScoringSummary
  runRecord$llmFieldReasons = jsonlite::toJSON(
    parsedScores$fieldReasons,
    auto_unbox = TRUE,
    null = "null"
  )

  runRecord
}
