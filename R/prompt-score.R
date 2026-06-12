#' Build the system prompt for WMFM explanation scoring
#'
#' Creates the fixed system prompt used when asking a language model to score a
#' plain-language model explanation against the WMFM scoring schema.
#'
#' @return A character scalar containing the system prompt.
#' @keywords internal
buildWmfmLlmScoringSystemPrompt = function() {

  paste(
    "You are scoring a plain-language explanation of a fitted statistical model.",
    "You must score the explanation against the supplied fitted-model context.",
    "Be strict, consistent, and conservative.",
    "",
    "Scoring rules:",
    "- Distinguish clearly between descriptive wording and inferential wording.",
    "- Penalise incorrect effect direction, incorrect effect scale, incorrect baseline/reference handling, and incorrect interaction interpretation.",
    "- Penalise causal language unless the supplied model context explicitly justifies causation.",
    "- If interaction terms are present, assess whether the explanation covers them adequately and correctly.",
    "- If interaction terms are absent, penalise invented interaction claims.",
    "- Penalise overclaiming more heavily than cautious wording.",
    "- Use only the information provided in the prompt.",
    "",
    "Output rules:",
    "- Return strict JSON only.",
    "- Do not wrap the JSON in markdown unless the provider forces it.",
    "- Use the exact field names requested.",
    "- For rubric fields scored on the 0/1/2 scale:",
    "  2 = correct, appropriate, adequate, or clear",
    "  1 = partly correct, incomplete, mixed, or somewhat unclear",
    "  0 = clearly wrong, inappropriate, missing when important, or seriously unclear",
    "",
    "Aggregate score rules:",
    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be numeric values between 0 and 2 inclusive.",
    "- overallScore must be a numeric value between 0 and 100 inclusive.",
    "- overallPass should usually be FALSE if fatalFlawDetected is TRUE.",
    sep = "\n"
  )
}

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

  adjustmentScoringPolicy = buildAdjustmentScoringPolicy(runRecord = runRecord)
  adjustmentContextBlock = buildAdjustmentScoringContextBlock(
    runRecord = runRecord,
    policy = adjustmentScoringPolicy
  )

  followupFields = c(
    "followupQuestion",
    "followupCategory",
    "followupPredictionStatus",
    "followupPredictionType",
    "followupIntervalType",
    "followupFutureObservationType",
    "followupExtrapolationStatus",
    "followupExtrapolationExplanation"
  )
  hasFollowupContext = isTRUE(runRecord$hasFollowupScoringContext) ||
    any(vapply(followupFields, function(field) {
      value = runRecord[[field]]
      length(value) > 0 && !is.na(value[1]) && nzchar(trimws(as.character(value[1])))
    }, logical(1)))

  followupContextBlock =
    if (hasFollowupContext) {
      paste(
        "Follow-up scoring context is present.",
        paste0("Follow-up question: ", safeWmfmScalar(runRecord$followupQuestion)),
        paste0("Follow-up category: ", safeWmfmScalar(runRecord$followupCategory)),
        paste0("Prediction status: ", safeWmfmScalar(runRecord$followupPredictionStatus)),
        paste0("Prediction type: ", safeWmfmScalar(runRecord$followupPredictionType)),
        paste0("Interval type: ", safeWmfmScalar(runRecord$followupIntervalType)),
        paste0("Future-observation type: ", safeWmfmScalar(runRecord$followupFutureObservationType)),
        paste0("Extrapolation status: ", safeWmfmScalar(runRecord$followupExtrapolationStatus)),
        paste0("Extrapolation explanation: ", safeWmfmScalar(runRecord$followupExtrapolationExplanation)),
        paste0("Parameter uncertainty included: ", safeWmfmScalar(runRecord$followupParameterUncertaintyIncluded)),
        "Scoring policy for follow-up explanations:",
        "- Distinguish fitted-mean confidence intervals from future-observation prediction intervals.",
        "- Reward correct extrapolation-warning or blocked-prediction wording when the context supplies it.",
        "- For Poisson follow-ups, treat future-count prediction intervals as uncertainty about a future count, not uncertainty about the fitted mean.",
        "- For logistic future outcomes, reward Bernoulli outcome-probability framing rather than continuous prediction-interval wording.",
        "- Penalise claims that parameter uncertainty is included when the context says it is not included.",
        "- Use uncertaintyHandlingAppropriate for correctness of interval type, future-observation uncertainty, blocked-prediction warnings, extrapolation warnings, and parameter-uncertainty caveats.",
        "- Use comparisonStructureClear for whether the explanation clearly separates fitted-mean, future-observation, Poisson-count, or Bernoulli-outcome structures when those distinctions are relevant.",
        "- Mark fatalFlawDetected TRUE when a follow-up answer uses confidence-interval wording for a prediction interval, treats a Bernoulli outcome as a continuous individual interval, ignores a blocked prediction, or claims included parameter uncertainty when it is excluded.",
        sep = "\n"
      )
    } else {
      "No follow-up prediction context was supplied. Use the standard model-explanation scoring expectations."
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
    "  2 if important interactions among variables of scientific interest are clearly covered when present; 1 if interaction coverage is partial; 0 if an important primary interaction is missed or an interaction is invented.",
    "  When adjustment variables are supplied, do not mark this field down merely because the explanation avoids adjustment-level interaction cell narration.",
    "- interactionSubstantiveCorrect:",
    "  2 if the explanation gets the substantive interpretation of the interaction right; 1 if partly right or vague; 0 if wrong.",
    "  When an interaction involves an adjustment variable, prefer high-level adjusted-for interpretation over level-specific adjustment-cell storytelling unless the context asks for those cells.",
    "- uncertaintyHandlingAppropriate:",
    "  2 if uncertainty or evidential qualification is handled appropriately, including correct follow-up interval type where supplied; 1 if present but limited; 0 if badly mishandled or absent in a way that encourages overstatement.",
    "  In follow-up scoring, this field should judge whether the answer correctly handles fitted-mean versus future-observation uncertainty, prediction-interval versus confidence-interval wording, extrapolation or blocked-prediction warnings, and whether parameter uncertainty is included or excluded.",
    "- inferentialRegisterAppropriate:",
    "  2 if the wording matches the evidential strength and avoids inappropriate causal claims; 1 if somewhat mixed; 0 if clearly overclaiming or otherwise inappropriate.",
    "- mainEffectCoverageAdequate:",
    "  2 if the key variables of scientific interest are adequately covered; 1 if partly covered; 0 if important primary variables are omitted.",
    "  When adjustment variables are supplied, do not require separate substantive narration of adjustment-variable main effects.",
    "- referenceGroupCoverageAdequate:",
    "  2 if the explanation adequately communicates the baseline or comparison structure for variables of scientific interest when relevant; 1 if partial; 0 if missing when important.",
    "  When adjustment variables are supplied, do not require adjustment-level reference-group narration unless it is needed to answer the research question.",
    "- clarityAdequate:",
    "  2 if the explanation is generally clear and easy to follow; 1 if understandable but somewhat awkward, wordy, or uneven; 0 if seriously unclear.",
    "- numericExpressionAdequate:",
    "  2 if the explanation gives a clear quantitative interpretation of one or more important primary model effects using meaningful numbers, units, point differences, probabilities, odds, multiplicative language, or intervals where appropriate.",
    "  1 if some numeric information is present but the quantitative interpretation is partial, weakly connected to the effect, vague, or somewhat unclear.",
    "  0 if meaningful quantitative interpretation of the variables of scientific interest is absent when it should be present, or if the numeric expression is seriously misleading.",
    "  Minor stylistic awkwardness alone should not reduce a score from 2 to 1 if the quantitative interpretation is still clear.",
    "  Do not treat percentage language about model fit, such as R-squared or percent of variation explained, as evidence that the coefficient effect scale is multiplicative.",
    "- comparisonStructureClear:",
    "  2 if relevant comparisons or conditional structures for the variables of scientific interest are clearly expressed; 1 if partly clear; 0 if important primary comparison structure is missing or confusing.",
    "  When adjustment variables are supplied, do not require adjustment-level comparison structures or conditioning axes unless they are needed to answer the research question.",
    "  In follow-up scoring, this field should also judge whether the answer clearly separates fitted-mean quantities from future-observation quantities, and whether Poisson count or Bernoulli outcome framing is structurally clear.",
    "- fatalFlawDetected:",
    "  TRUE only for a serious problem such as a major directional error, clear overclaiming, invented interaction, or another flaw that should strongly affect the final judgment.",
    "  In follow-up scoring, examples include using confidence-interval wording for an individual prediction interval, treating Bernoulli outcomes as continuous individual intervals, ignoring a blocked prediction, or claiming parameter uncertainty is included when the context says it is excluded.",
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
    "ADJUSTMENT CONTEXT",
    adjustmentContextBlock,
    "",
    "FOLLOW-UP SCORING CONTEXT",
    followupContextBlock,
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
