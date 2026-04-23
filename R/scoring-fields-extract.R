#' Extract score fields from a scored WMFM run
#'
#' Given a one-row scored run record, keep only the judged quality fields,
#' aggregate scores, and selected scoring metadata.
#'
#' @param x A named list or one-row data frame containing scored run data.
#'
#' @return A named list containing score fields only.
#'
#' @keywords internal
#' @noRd
extractWmfmScoreFields = function(x) {

  if (is.data.frame(x)) {
    if (nrow(x) != 1) {
      stop("`x` must be a one-row data frame.", call. = FALSE)
    }

    x = as.list(x[1, , drop = FALSE])
  }

  if (!is.list(x) || is.null(names(x))) {
    stop("`x` must be a named list or one-row data frame.", call. = FALSE)
  }

  keepFields = c(
    "interactionEvidenceAppropriate",
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
    "comparisonStructureClear",
    "fatalFlawDetected",
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore",
    "overallScore",
    "overallPass",
    "duplicateCount",
    "isExactDuplicate",
    "duplicatePenaltyApplied",
    "llmScored",
    "llmScoringModel",
    "llmScoringRaw",
    "llmScoringSummary",
    "llmFieldReasons",
    "primaryScoringMethod"
  )

  out = x[intersect(keepFields, names(x))]

  emptyFields = setdiff(keepFields, names(out))
  if (length(emptyFields) > 0) {
    for (fieldName in emptyFields) {
      out[[fieldName]] = NULL
    }
  }

  out
}
