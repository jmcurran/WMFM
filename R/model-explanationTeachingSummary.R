#' Build a student-facing teaching summary for a model explanation
#'
#' Derives a plain-language teaching summary from the deterministic explanation
#' audit. The returned object is designed for student-facing display and avoids
#' exposing raw prompt ingredients or developer-facing internal structures.
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param researchQuestion Optional character string giving the research
#'   question the model is being used to address. When `NULL`, the function
#'   falls back to any research question stored on the fitted model.
#'
#' @return An object of class `wmfmExplanationTeachingSummary`.
#' @export
#' @importFrom stats formula model.frame
buildExplanationTeachingSummary = function(audit, model, researchQuestion = NULL) {

  if (is.null(audit)) {
    stop("`audit` must not be NULL.", call. = FALSE)
  }

  if (!inherits(model, c("lm", "glm"))) {
    stop("`model` must be a fitted lm or glm object.", call. = FALSE)
  }

  if (is.null(researchQuestion)) {
    researchQuestion = trimws(attr(model, "wmfm_research_question", exact = TRUE) %||% "")
  }

  if (!is.character(researchQuestion) || length(researchQuestion) != 1 || is.na(researchQuestion)) {
    stop("`researchQuestion` must be NULL or a single non-missing character string.", call. = FALSE)
  }

  mf = stats::model.frame(model)
  responseName = all.vars(stats::formula(model)[[2]])[1] %||% names(mf)[1] %||% "the response"
  predictorNames = names(mf)[-1]
  numericPredictors = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]
  factorPredictors = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]

  out = list(
    interpretationScale = buildExplanationTeachingInterpretationScale(audit = audit),
    baselineChoice = buildExplanationTeachingBaselineChoice(
      audit = audit,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors
    ),
    xChangeDescription = buildExplanationTeachingXChangeDescription(
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors,
      audit = audit
    ),
    mainEffectDescription = buildExplanationTeachingMainEffectDescription(
      audit = audit,
      responseName = responseName,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors
    ),
    uncertaintySummary = buildExplanationTeachingUncertaintySummary(audit = audit),
    evidenceTable = buildExplanationTeachingEvidenceTable(
      audit = audit,
      responseName = responseName,
      numericPredictors = numericPredictors,
      factorPredictors = factorPredictors,
      researchQuestion = researchQuestion
    ),
    researchQuestionLink = buildExplanationTeachingResearchQuestionLink(
      researchQuestion = researchQuestion,
      responseName = responseName
    )
  )

  class(out) = c("wmfmExplanationTeachingSummary", class(out))
  out
}

#' Build the interpretation-scale teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingInterpretationScale = function(audit) {

  fittedScale = audit$interpretationScale$fittedValueScale %||% "response scale"
  effectScale = audit$interpretationScale$effectScale %||% "interpretation scale"
  backTransformation = audit$interpretationScale$backTransformation %||% ""

  if (nzchar(backTransformation) && !identical(backTransformation, "No back-transformation is required.")) {
    return(paste(
      "The explanation was written on a student-friendly scale.",
      "Fitted values were interpreted on the",
      fittedScale,
      "and effects were described as",
      effectScale,
      "rather than leaving them on the raw coefficient scale.",
      backTransformation
    ))
  }

  paste(
    "The explanation was written on a student-friendly scale.",
    "Fitted values were interpreted on the",
    fittedScale,
    "and effects were described as",
    effectScale,
    "rather than leaving them on the raw coefficient scale."
  )
}

#' Build the baseline-choice teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingBaselineChoice = function(audit, numericPredictors, factorPredictors) {

  parts = character(0)

  if (length(numericPredictors) > 0 && is.data.frame(audit$numericAnchor$table) && nrow(audit$numericAnchor$table) > 0) {
    numericText = apply(audit$numericAnchor$table, 1, function(row) {
      paste0(
        row[["predictor"]],
        " was anchored at ",
        row[["anchor"]],
        " because ",
        tolower(row[["reason"]])
      )
    })

    parts = c(
      parts,
      paste(
        "For numeric predictors, the app chose sensible reference values instead of automatically using 0.",
        paste(numericText, collapse = " ")
      )
    )
  }

  if (length(factorPredictors) > 0 && is.data.frame(audit$referenceLevels) && nrow(audit$referenceLevels) > 0) {
    factorText = apply(audit$referenceLevels, 1, function(row) {
      paste0(
        row[["predictor"]],
        " used ",
        row[["referenceLevel"]],
        " as the comparison level"
      )
    })

    parts = c(
      parts,
      paste(
        "For categorical predictors, the explanation compared groups against reference levels.",
        paste(factorText, collapse = "; "),
        "."
      )
    )
  }

  if (length(parts) == 0) {
    return("No special baseline choice was needed because there were no numeric or categorical comparison settings to explain.")
  }

  paste(parts, collapse = " ")
}

#' Build the x-change teaching text
#'
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingXChangeDescription = function(numericPredictors, factorPredictors, audit) {

  parts = character(0)

  if (length(numericPredictors) > 0) {
    parts = c(
      parts,
      paste0(
        "For numeric predictors such as ",
        paste(numericPredictors, collapse = ", "),
        ", the explanation treats the main coefficient as the change associated with a one-unit increase, while keeping the other variables at their chosen reference settings."
      )
    )
  }

  if (length(factorPredictors) > 0) {
    parts = c(
      parts,
      paste0(
        "For categorical predictors such as ",
        paste(factorPredictors, collapse = ", "),
        ", the explanation treats the effect as a comparison with the reference group rather than as a one-unit increase."
      )
    )
  }

  if (length(parts) == 0) {
    parts = c(
      parts,
      "The explanation focuses on model-based comparisons, but there were no predictor changes that needed special handling here."
    )
  }

  if (nzchar(audit$interpretationScale$explanationScaleNote %||% "")) {
    parts = c(parts, audit$interpretationScale$explanationScaleNote)
  }

  paste(parts, collapse = " ")
}

#' Build the main-effect teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingMainEffectDescription = function(audit, responseName, numericPredictors, factorPredictors) {

  effectQuantities = character(0)

  if (is.data.frame(audit$effectEvidence) && nrow(audit$effectEvidence) > 0 && "quantity" %in% names(audit$effectEvidence)) {
    effectQuantities = unique(stats::na.omit(as.character(audit$effectEvidence$quantity)))
  }

  scaleText = audit$interpretationScale$effectScale %||% "student-friendly effect summaries"

  if (length(effectQuantities) > 0) {
    return(paste(
      "The main effect description was based on model-derived quantities rather than on the raw regression table alone.",
      "In particular, the app used",
      paste(effectQuantities, collapse = ", "),
      "to explain how",
      responseName,
      "changes on the",
      scaleText,
      "."
    ))
  }

  if (length(numericPredictors) > 0 || length(factorPredictors) > 0) {
    return(paste(
      "The main effect description was built by translating the fitted model into plain language.",
      "Instead of repeating coefficient jargon, the app summarised how the predictors relate to",
      responseName,
      "on the",
      scaleText,
      "."
    ))
  }

  paste(
    "The main effect description was built from the fitted model in plain language so that the explanation stays focused on what happens to",
    responseName,
    "rather than on technical coefficient details."
  )
}

#' Build the uncertainty teaching text
#'
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingUncertaintySummary = function(audit) {

  confidenceLevel = round((audit$confidenceIntervals$level %||% 0.95) * 100)
  displayedScales = audit$confidenceIntervals$displayedScales %||% character(0)
  displayedScales = displayedScales[!is.na(displayedScales) & nzchar(displayedScales)]

  summaryText = paste(
    "Uncertainty was handled using",
    confidenceLevel,
    "% confidence intervals.",
    "These intervals were used to support cautious statements about the likely direction and size of effects, rather than to present the model as exact."
  )

  if (length(displayedScales) > 0) {
    summaryText = paste(
      summaryText,
      "Where possible, the intervals were shown on the same scale used for interpretation:",
      paste(displayedScales, collapse = ", "),
      "."
    )
  }

  teachingNote = audit$confidenceIntervals$teachingNote %||% audit$confidenceIntervals$note %||% ""

  if (nzchar(teachingNote)) {
    summaryText = paste(summaryText, teachingNote)
  }

  summaryText
}

#' Build the teaching evidence table
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param responseName Name of the response variable.
#' @param numericPredictors Character vector of numeric predictor names.
#' @param factorPredictors Character vector of factor predictor names.
#' @param researchQuestion Optional research question string.
#'
#' @return A data frame.
#' @keywords internal
buildExplanationTeachingEvidenceTable = function(
    audit,
    responseName,
    numericPredictors,
    factorPredictors,
    researchQuestion
) {

  rows = list(
    data.frame(
      section = "Interpretation scale",
      summary = paste(
        "Explain",
        responseName,
        "on the",
        audit$interpretationScale$fittedValueScale %||% "response scale",
        "using",
        audit$interpretationScale$effectScale %||% "student-friendly effect summaries",
        "."
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Reference settings",
      summary = paste(
        "Use anchors for numeric predictors:",
        if (length(numericPredictors) > 0) paste(numericPredictors, collapse = ", ") else "none",
        "; use reference levels for factors:",
        if (length(factorPredictors) > 0) paste(factorPredictors, collapse = ", ") else "none",
        "."
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Effect evidence",
      summary = if (is.data.frame(audit$effectEvidence) && nrow(audit$effectEvidence) > 0) {
        paste(
          "Use model-derived quantities:",
          paste(unique(as.character(audit$effectEvidence$quantity)), collapse = ", "),
          "."
        )
      } else {
        "Use the fitted model to describe the main effect in plain language."
      },
      stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Uncertainty",
      summary = paste0(
        "Use ",
        round((audit$confidenceIntervals$level %||% 0.95) * 100),
        "% confidence intervals to describe the likely direction and size of effects cautiously."
      ),
      stringsAsFactors = FALSE
    )
  )

  if (nzchar(researchQuestion)) {
    rows[[length(rows) + 1]] = data.frame(
      section = "Research question",
      summary = "Use the student or teacher research question to frame the explanation and return to it at the end.",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

#' Build the research-question teaching text
#'
#' @param researchQuestion Optional research question string.
#' @param responseName Name of the response variable.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationTeachingResearchQuestionLink = function(researchQuestion, responseName) {

  if (!nzchar(trimws(researchQuestion))) {
    return(paste(
      "No separate research question was supplied, so the explanation stays focused on how the predictors relate to",
      responseName,
      "."
    ))
  }

  paste(
    "The explanation is linked back to the research question so that the model output answers a teaching goal, not just a statistical one.",
    "Here, the explanation should help the student judge what the fitted model says about this question:",
    researchQuestion
  )
}
