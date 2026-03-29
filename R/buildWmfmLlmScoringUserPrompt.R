#' Build a user prompt for WMFM explanation scoring
#'
#' Given a single run record produced by `buildWmfmRunRecord()`, construct a
#' prompt asking a language model to score the explanation and return strict
#' JSON matching the WMFM scoring schema.
#'
#' @param runRecord Named list produced by `buildWmfmRunRecord()`.
#'
#' @return A character scalar containing the prompt text.
#' @keywords internal
buildWmfmLlmScoringUserPrompt = function(runRecord) {

  if (!is.list(runRecord)) {
    stop("`runRecord` must be a named list.", call. = FALSE)
  }

  requiredFields = c(
    "exampleName",
    "package",
    "modelType",
    "formula",
    "equationsText",
    "explanationText",
    "interactionTerms",
    "hasInteractionTerms",
    "nInteractionTerms",
    "interactionMinPValue",
    "interactionAlpha"
  )

  missingFields = setdiff(requiredFields, names(runRecord))
  if (length(missingFields) > 0) {
    stop(
      "runRecord is missing required fields: ",
      paste(missingFields, collapse = ", "),
      call. = FALSE
    )
  }

  interactionBlock =
    if (isTRUE(runRecord$hasInteractionTerms)) {
      paste(
        "Interaction terms are present in the fitted model.",
        paste0("Interaction terms: ", safeWmfmScalar(runRecord$interactionTerms)),
        paste0("Number of interaction terms: ", safeWmfmScalar(runRecord$nInteractionTerms)),
        paste0("Minimum interaction p-value: ", safeWmfmScalar(runRecord$interactionMinPValue)),
        paste0("Interaction alpha threshold: ", safeWmfmScalar(runRecord$interactionAlpha)),
        sep = "\n"
      )
    } else {
      "No interaction terms are present in the fitted model."
    }

  jsonTemplate = paste(
    "{",
    '  "effectDirectionCorrect": 0,',
    '  "effectScaleAppropriate": 0,',
    '  "referenceGroupHandledCorrectly": 0,',
    '  "interactionCoverageAdequate": 0,',
    '  "interactionSubstantiveCorrect": 0,',
    '  "uncertaintyHandlingAppropriate": 0,',
    '  "inferentialRegisterAppropriate": 0,',
    '  "mainEffectCoverageAdequate": 0,',
    '  "referenceGroupCoverageAdequate": 0,',
    '  "clarityAdequate": 0,',
    '  "numericExpressionAdequate": 0,',
    '  "comparisonStructureClear": 0,',
    '  "fatalFlawDetected": false,',
    '  "factualScore": 0.0,',
    '  "inferenceScore": 0.0,',
    '  "completenessScore": 0.0,',
    '  "clarityScore": 0.0,',
    '  "calibrationScore": 0.0,',
    '  "overallScore": 0.0,',
    '  "overallPass": false,',
    '  "llmScoringSummary": "Short summary.",',
    '  "fieldReasons": {',
    '    "effectDirectionCorrect": "brief reason",',
    '    "effectScaleAppropriate": "brief reason",',
    '    "referenceGroupHandledCorrectly": "brief reason",',
    '    "interactionCoverageAdequate": "brief reason",',
    '    "interactionSubstantiveCorrect": "brief reason",',
    '    "uncertaintyHandlingAppropriate": "brief reason",',
    '    "inferentialRegisterAppropriate": "brief reason",',
    '    "mainEffectCoverageAdequate": "brief reason",',
    '    "referenceGroupCoverageAdequate": "brief reason",',
    '    "clarityAdequate": "brief reason",',
    '    "numericExpressionAdequate": "brief reason",',
    '    "comparisonStructureClear": "brief reason",',
    '    "fatalFlawDetected": "brief reason"',
    '  }',
    "}",
    sep = "\n"
  )

  paste(
    "Please score the following WMFM explanation.",
    "",
    "MODEL CONTEXT",
    paste0("Example name: ", safeWmfmScalar(runRecord$exampleName)),
    paste0("Package: ", safeWmfmScalar(runRecord$package)),
    paste0("Model type: ", safeWmfmScalar(runRecord$modelType)),
    paste0("Formula: ", safeWmfmScalar(runRecord$formula)),
    "",
    "FITTED EQUATIONS",
    safeWmfmScalar(runRecord$equationsText),
    "",
    "INTERACTION CONTEXT",
    interactionBlock,
    "",
    "EXPLANATION TO SCORE",
    safeWmfmScalar(runRecord$explanationText),
    "",
    "Return strict JSON with exactly these top-level fields:",
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
    "llmScoringSummary",
    "fieldReasons",
    "",
    "Use this JSON shape:",
    jsonTemplate,
    "",
    "Important reminders:",
    "- Do not add any extra top-level fields.",
    "- Do not omit required fields.",
    "- fieldReasons must itself be a JSON object with the required reason fields.",
    "- Keep reasons brief.",
    "- Return JSON only.",
    sep = "\n"
  )
}
