#' Build a user prompt for WMFM explanation scoring
#'
#' Given a single run record produced by `buildWmfmRunRecord()`, construct a
#' prompt asking a language model to score the explanation and return strict
#' JSON matching the WMFM scoring schema.
#'
#' This prompt includes field-specific rubric guidance so that the language
#' model applies the WMFM scoring dimensions consistently rather than inferring
#' its own definitions of what counts as adequate, clear, or appropriate.
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

  fieldSpecificRubric = paste(
    "FIELD-SPECIFIC RUBRIC GUIDANCE",
    "- effectDirectionCorrect:",
    "  2 if the explanation gives the correct direction of the main effect(s); 1 if only partly correct, mixed, or incomplete; 0 if direction is wrong or missing when important.",
    "- effectScaleAppropriate:",
    "  2 if the explanation uses the correct effect scale for the model context (for example additive differences in points, multiplicative changes, probabilities, or odds); 1 if the scale is only partly clear or mixed; 0 if the scale is wrong, seriously confused, or absent when important.",
    "- referenceGroupHandledCorrectly:",
    "  2 if the baseline or comparison group is handled correctly; 1 if only partly clear; 0 if the baseline or comparison is wrong or materially misleading.",
    "- interactionCoverageAdequate:",
    "  2 if important interactions are clearly covered when present; 1 if interaction coverage is partial; 0 if an important interaction is missed or an interaction is invented.",
    "- interactionSubstantiveCorrect:",
    "  2 if the explanation gets the substantive interpretation of the interaction right; 1 if partly right or vague; 0 if wrong.",
    "- uncertaintyHandlingAppropriate:",
    "  2 if uncertainty or evidential qualification is handled appropriately; 1 if present but limited; 0 if badly mishandled or absent in a way that encourages overstatement.",
    "- inferentialRegisterAppropriate:",
    "  2 if the wording matches the evidential strength and avoids inappropriate causal claims; 1 if somewhat mixed; 0 if clearly overclaiming or otherwise inappropriate.",
    "- mainEffectCoverageAdequate:",
    "  2 if the key main effects are adequately covered; 1 if partly covered; 0 if important main effects are omitted.",
    "- referenceGroupCoverageAdequate:",
    "  2 if the explanation adequately communicates the baseline or comparison structure when relevant; 1 if partial; 0 if missing when important.",
    "- clarityAdequate:",
    "  2 if the explanation is generally clear and easy to follow; 1 if understandable but somewhat awkward, wordy, or uneven; 0 if seriously unclear.",
    "- numericExpressionAdequate:",
    "  2 if the explanation gives a clear quantitative interpretation of one or more important model effects using meaningful numbers, units, point differences, probabilities, odds, multiplicative language, or intervals where appropriate.",
    "  1 if some numeric information is present but the quantitative interpretation is partial, weakly connected to the effect, vague, or somewhat unclear.",
    "  0 if meaningful quantitative interpretation is absent when it should be present, or if the numeric expression is seriously misleading.",
    "  Minor stylistic awkwardness alone should not reduce a score from 2 to 1 if the quantitative interpretation is still clear.",
    "  Do not treat percentage language about model fit, such as R-squared or percent of variation explained, as evidence that the coefficient effect scale is multiplicative.",
    "- comparisonStructureClear:",
    "  2 if relevant comparisons or conditional structures are clearly expressed; 1 if partly clear; 0 if important comparison structure is missing or confusing.",
    "- fatalFlawDetected:",
    "  TRUE only for a serious problem such as a major directional error, clear overclaiming, invented interaction, or another flaw that should strongly affect the final judgment.",
    sep = "\n"
  )

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
    fieldSpecificRubric,
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
    "Important score-scale reminder:",
    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be between 0 and 2.",
    "- overallScore must be between 0 and 100.",
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
