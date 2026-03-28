#' Explain why a WMFM field received its score for a specific run
#'
#' Provides a human-readable explanation of how a particular field
#' (claim, judged field, or score) was derived for a given run.
#'
#' This function works directly with a repeated-runs object and will
#' internally call `scoreWmfmRepeatedRuns()` if needed.
#'
#' @param field Character. Field name or plotted label (e.g. "Overclaim",
#'   "Interaction evidence", "overallScore").
#' @param x A repeated-runs object (output of runWMFMPackageExampleRepeated)
#'   or a data.frame containing run records.
#' @param runIndex Integer. Which run to explain.
#'
#' @return Invisibly returns a character vector explanation. Also prints
#'   to console using cat().
#' @export
explainWmfmFieldScore = function(field, x, runIndex = 1L) {

  # ---- helper: extract runsDf ----
  extractRunsDf = function(obj) {
    if (is.data.frame(obj)) {
      return(obj)
    }
    if (is.list(obj) && "runsDf" %in% names(obj)) {
      return(obj$runsDf)
    }
    stop("`x` must be a repeated-runs object or a data.frame.", call. = FALSE)
  }

  # ---- helper: map pretty names to internal ----
  fieldMap = c(
    "Direction claim" = "effectDirectionClaim",
    "Scale claim" = "effectScaleClaim",
    "Reference mention" = "referenceGroupMention",
    "Interaction mention" = "interactionMention",
    "Interaction claim" = "interactionSubstantiveClaim",
    "Uncertainty mention" = "uncertaintyMention",
    "Uncertainty type" = "uncertaintyTypeClaim",
    "Inferential words" = "usesInferentialLanguage",
    "Descriptive-only words" = "usesDescriptiveOnlyLanguage",
    "Overclaim" = "overclaimDetected",
    "Underclaim" = "underclaimDetected",
    "Register" = "inferentialRegister",
    "CI mention" = "ciMention",
    "% language" = "percentLanguageMention",
    "Conditional language" = "conditionalLanguageMention",
    "Comparison language" = "comparisonLanguageMention",
    "Interaction evidence" = "interactionEvidenceAppropriate",
    "Direction correct" = "effectDirectionCorrect",
    "Scale appropriate" = "effectScaleAppropriate",
    "Reference correct" = "referenceGroupHandledCorrectly",
    "Interaction coverage" = "interactionCoverageAdequate",
    "Interaction correct" = "interactionSubstantiveCorrect",
    "Uncertainty handling" = "uncertaintyHandlingAppropriate",
    "Register appropriate" = "inferentialRegisterAppropriate",
    "Main-effect coverage" = "mainEffectCoverageAdequate",
    "Reference coverage" = "referenceGroupCoverageAdequate",
    "Clarity" = "clarityAdequate",
    "Numbers" = "numericExpressionAdequate",
    "Comparison structure" = "comparisonStructureClear",
    "Fatal flaw" = "fatalFlawDetected",
    "Overall pass" = "overallPass",
    "Factual score" = "factualScore",
    "Inference score" = "inferenceScore",
    "Completeness score" = "completenessScore",
    "Clarity score" = "clarityScore",
    "Calibration score" = "calibrationScore",
    "Overall score" = "overallScore"
  )

  if (field %in% names(fieldMap)) {
    fieldInternal = fieldMap[[field]]
  } else {
    fieldInternal = field
  }

  runsDf = extractRunsDf(x)

  if (runIndex < 1 || runIndex > nrow(runsDf)) {
    stop("`runIndex` is out of bounds.", call. = FALSE)
  }

  # ---- ensure scoring present ----
  if (!("overallScore" %in% names(runsDf))) {
    runsDf = scoreWmfmRepeatedRuns(runsDf)
  }

  row = runsDf[runIndex, , drop = FALSE]

  explanation = row$explanationText
  value = row[[fieldInternal]]

  out = c(
    paste0("Run: ", runIndex),
    paste0("Field: ", field),
    paste0("Internal field: ", fieldInternal),
    ""
  )

  # ---- logic by field ----

  if (fieldInternal == "overclaimDetected") {
    out = c(out,
            paste0("Detected value: ", value),
            if (isTRUE(value)) {
              "The explanation contains strong causal or definitive language (e.g. 'proves', 'causes')."
            } else {
              "No strong overclaiming language detected."
            }
    )
  }

  else if (fieldInternal == "interactionEvidenceAppropriate") {
    out = c(out,
            paste0("Detected value: ", value),
            paste0("Interaction p-value: ", row$interactionMinPValue),
            paste0("Interaction claim: ", row$interactionSubstantiveClaim),
            "",
            "Interpretation:",
            switch(as.character(value),
                   appropriate = "The explanation's claim about interaction matches the statistical evidence.",
                   too_strong = "The explanation claims a strong interaction not supported by the model.",
                   too_weak = "The explanation fails to acknowledge a real interaction.",
                   unclear = "Unable to determine appropriateness.",
                   not_applicable = "No interaction terms in model."
            )
    )
  }

  else if (fieldInternal %in% c("effectDirectionCorrect", "effectScaleAppropriate")) {
    out = c(out,
            paste0("Score: ", value, " (0 = incorrect, 1 = partial, 2 = correct)"),
            "This reflects whether the explanation correctly described the direction/scale of the effect."
    )
  }

  else if (fieldInternal == "overallScore") {
    out = c(out,
            paste0("Overall score: ", value),
            paste0("Factual: ", row$factualScore),
            paste0("Inference: ", row$inferenceScore),
            paste0("Completeness: ", row$completenessScore),
            paste0("Clarity: ", row$clarityScore),
            paste0("Calibration: ", row$calibrationScore),
            "",
            "This is a weighted combination of the five scoring dimensions."
    )
  }

  else if (fieldInternal %in% names(row)) {
    out = c(out,
            paste0("Value: ", value),
            "This field is part of the extracted or judged schema."
    )
  }

  else {
    out = c(out, "Field not recognised.")
  }

  # ---- include snippet of explanation ----
  if (!is.null(explanation) && nzchar(explanation)) {
    out = c(out, "", "Explanation text (truncated):",
            substr(explanation, 1, 300))
  }

  cat(paste(out, collapse = "\n"))
  invisible(out)
}
