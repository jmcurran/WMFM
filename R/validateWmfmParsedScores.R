#' Validate parsed WMFM LLM scores
#'
#' Validates the structure and allowable values of a parsed JSON scoring
#' response before it is written back into a run record.
#'
#' @param parsedScores Named list produced by `parseWmfmScoringJson()`.
#'
#' @return The validated and type-normalised score list.
#' @keywords internal
validateWmfmParsedScores = function(parsedScores) {

  if (!is.list(parsedScores) || is.null(names(parsedScores))) {
    stop("`parsedScores` must be a named list.", call. = FALSE)
  }

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

  logicalFields = c(
    "fatalFlawDetected",
    "overallPass"
  )

  characterFields = c(
    "llmScoringSummary"
  )

  requiredReasonFields = c(
    rubricFields,
    "fatalFlawDetected"
  )

  requiredFields = c(
    rubricFields,
    numericFields,
    logicalFields,
    characterFields,
    "fieldReasons"
  )

  missingFields = setdiff(requiredFields, names(parsedScores))
  if (length(missingFields) > 0) {
    stop(
      "Parsed scoring response is missing required fields: ",
      paste(missingFields, collapse = ", "),
      call. = FALSE
    )
  }

  out = parsedScores

  for (field in rubricFields) {
    value = suppressWarnings(as.integer(out[[field]]))
    if (length(value) != 1 || is.na(value) || !(value %in% c(0L, 1L, 2L))) {
      stop(
        "Field `", field, "` must be one of 0, 1, or 2.",
        call. = FALSE
      )
    }
    out[[field]] = value
  }

  for (field in numericFields) {
    value = suppressWarnings(as.numeric(out[[field]]))
    if (length(value) != 1 || is.na(value) || !is.finite(value)) {
      stop("Field `", field, "` must be a finite numeric scalar.", call. = FALSE)
    }
    if (value < 0 || value > 2) {
      stop("Field `", field, "` must lie between 0 and 2.", call. = FALSE)
    }
    out[[field]] = value
  }

  for (field in logicalFields) {
    value = out[[field]]
    if (is.character(value)) {
      lowerValue = tolower(trimws(value[1]))
      if (lowerValue %in% c("true", "t")) {
        value = TRUE
      } else if (lowerValue %in% c("false", "f")) {
        value = FALSE
      }
    }
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
      stop("Field `", field, "` must be TRUE or FALSE.", call. = FALSE)
    }
    out[[field]] = value
  }

  for (field in characterFields) {
    value = safeWmfmScalar(out[[field]], naString = "")
    if (!nzchar(value)) {
      stop("Field `", field, "` must be a non-empty character scalar.", call. = FALSE)
    }
    out[[field]] = value
  }

  reasons = out$fieldReasons
  if (!is.list(reasons) || is.null(names(reasons))) {
    stop("Field `fieldReasons` must be a named JSON object.", call. = FALSE)
  }

  missingReasonFields = setdiff(requiredReasonFields, names(reasons))
  if (length(missingReasonFields) > 0) {
    stop(
      "fieldReasons is missing required fields: ",
      paste(missingReasonFields, collapse = ", "),
      call. = FALSE
    )
  }

  reasons = lapply(reasons, function(x) {
    safeWmfmScalar(x, naString = "", singleLine = TRUE)
  })

  emptyReasonFields = names(reasons)[names(reasons) %in% requiredReasonFields & !nzchar(unlist(reasons))]
  if (length(emptyReasonFields) > 0) {
    stop(
      "fieldReasons contains empty required fields: ",
      paste(emptyReasonFields, collapse = ", "),
      call. = FALSE
    )
  }

  out$fieldReasons = reasons
  out
}
