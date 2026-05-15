#' Build adjustment-variable guidance for explanation prompts
#'
#' Builds a prompt block that separates primary predictors from adjustment
#' variables using Stage 20 adjustment metadata stored on the fitted model.
#' The block instructs downstream explanation generation to interpret primary
#' predictors substantively while mentioning adjustment variables only as
#' adjusted-for controls.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing adjustment-variable guidance, or an
#'   empty string when no adjustment variables are present.
#' @keywords internal
#' @importFrom stats formula model.frame
buildAdjustmentVariablePromptBlock = function(model, mf = NULL) {

  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)

  if (!isTRUE(roleMetadata$hasAdjustments)) {
    return("")
  }

  primaryText = if (length(roleMetadata$primaryPredictors) > 0) {
    paste(roleMetadata$primaryPredictors, collapse = ", ")
  } else {
    "(none listed)"
  }

  adjustmentText = paste(roleMetadata$adjustmentPredictors, collapse = ", ")

  adjustmentPhrase = paste0("after adjusting for ", adjustmentText)
  omittedTerms = getAdjustmentRelatedTermsForExplanation(model = model, mf = mf)
  omittedTermsText = if (length(omittedTerms) > 0) {
    paste(omittedTerms, collapse = ", ")
  } else {
    "(none)"
  }

  hasAdjustmentInteractions = any(grepl(":", omittedTerms, fixed = TRUE))

  lines = c(
    "Interpretation policy:",
    paste0("Response variable: ", names(mf %||% stats::model.frame(model))[[1]]),
    paste0("Primary predictors: ", primaryText),
    paste0("The following variables are adjustment variables: ", adjustmentText),
    paste0("Omitted adjustment-related terms in explanation payload: ", omittedTermsText),
    paste0("Frame the main answer around primary predictors ", adjustmentPhrase, "."),
    "The research question is about the non-adjustment variables of interest.",
    "Interpret primary predictors as the substantive findings of interest.",
    "Mention adjustment variables only in adjusted-for language such as after adjusting for ... or after accounting for ....",
    "Do not interpret adjustment-variable coefficients, contrasts, confidence intervals, fitted means, predicted values, or model terms as findings.",
    "Do not discuss results separately by levels or values of adjustment variables.",
    "Do not use adjustment variables as narrative axes.",
    "Do not interpret interactions involving adjustment variables level by level.",
    "Do not present adjustment variables as causal mechanisms and do not infer causality from adjustment."
  )

  if (isTRUE(hasAdjustmentInteractions)) {
    lines = c(
      lines,
      "Model-structure note: The fitted model includes terms involving adjustment variables, so the adjusted comparison is based on that model structure."
    )
  }

  paste(lines, collapse = "\n")
}

#' Get adjustment-related terms for explanation-policy omission metadata
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return Character vector of model terms involving adjustment variables.
#' @keywords internal
getAdjustmentRelatedTermsForExplanation = function(model, mf = NULL) {
  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)
  if (!isTRUE(roleMetadata$hasAdjustments)) {
    return(character(0))
  }

  termLabels = attr(stats::terms(model), "term.labels") %||% character(0)
  termLabels = unique(as.character(termLabels))
  termLabels[nzchar(termLabels) & vapply(
    termLabels,
    termInvolvesAdjustmentVariable,
    logical(1),
    adjustmentVariables = roleMetadata$adjustmentPredictors
  )]
}

#' Get explanation-role metadata for primary and adjustment predictors
#'
#' Uses fitted-model predictor names plus Stage 20 adjustment metadata to build
#' a consistent role split for explanation and prompt helpers.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A list with `primaryPredictors`, `adjustmentPredictors`, and
#'   `hasAdjustments` fields.
#' @keywords internal
getAdjustmentRoleMetadataForExplanation = function(model, mf = NULL) {

  if (is.null(mf)) {
    mf = tryCatch(
      model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  predictorNames = if (!is.null(mf) && is.data.frame(mf) && ncol(mf) > 1) {
    names(mf)[-1]
  } else {
    all.vars(formula(model))[-1]
  }

  predictorNames = unique(as.character(predictorNames))
  predictorNames = predictorNames[nzchar(predictorNames)]

  adjustmentPredictors = attr(model, "wmfm_adjustment_variables", exact = TRUE) %||% character(0)
  adjustmentPredictors = unique(as.character(adjustmentPredictors))
  adjustmentPredictors = adjustmentPredictors[nzchar(adjustmentPredictors)]
  adjustmentPredictors = adjustmentPredictors[adjustmentPredictors %in% predictorNames]

  primaryPredictors = setdiff(predictorNames, adjustmentPredictors)

  list(
    primaryPredictors = primaryPredictors,
    adjustmentPredictors = adjustmentPredictors,
    hasAdjustments = length(adjustmentPredictors) > 0
  )
}

#' Build deterministic explanation scaffold for adjustment workflows
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing a deterministic adjusted-effect
#'   summary for primary variables, or an empty string when a safe summary
#'   cannot be constructed.
#' @keywords internal
#' @importFrom stats coef vcov qt
getAdjustedPrimaryEffectSummary = function(model, mf = NULL) {
  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)
  if (!isTRUE(roleMetadata$hasAdjustments)) {
    return("")
  }

  termLabels = attr(stats::terms(model), "term.labels") %||% character(0)
  hasPrimaryInteractions = any(vapply(
    as.character(termLabels),
    function(termLabel) {
      termParts = trimws(strsplit(as.character(termLabel), ":", fixed = TRUE)[[1]])
      any(termParts %in% roleMetadata$primaryPredictors) && length(termParts) > 1
    },
    logical(1)
  ))

  if (isTRUE(hasPrimaryInteractions) || length(roleMetadata$primaryPredictors) != 1) {
    return("")
  }

  primaryVariable = roleMetadata$primaryPredictors[[1]]
  estimateNames = names(stats::coef(model)) %||% character(0)
  candidateTerms = estimateNames[startsWith(estimateNames, paste0(primaryVariable))]

  if (length(candidateTerms) != 1) {
    return("")
  }

  termName = candidateTerms[[1]]
  estimate = unname(stats::coef(model)[[termName]])
  vcovMat = stats::vcov(model)
  if (is.null(vcovMat) || !(termName %in% rownames(vcovMat))) {
    return("")
  }

  se = sqrt(vcovMat[termName, termName])
  if (!is.finite(se) || se <= 0) {
    return("")
  }

  criticalValue = stats::qt(0.975, df = stats::df.residual(model))
  lower = estimate - criticalValue * se
  upper = estimate + criticalValue * se

  paste0(
    "Adjusted primary-effect summary: For ",
    primaryVariable,
    ", the adjusted model estimate is ",
    sprintf("%.2f", estimate),
    " (95% CI ",
    sprintf("%.2f", lower),
    " to ",
    sprintf("%.2f", upper),
    ")."
  )
}

#' Build deterministic explanation scaffold for adjustment workflows
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing a deterministic scaffold, or an empty
#'   string when no adjustment variables are selected.
#' @keywords internal
#' @importFrom stats terms
buildAdjustmentExplanationScaffold = function(model, mf = NULL) {
  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)
  if (!isTRUE(roleMetadata$hasAdjustments)) {
    return("")
  }

  if (is.null(mf)) {
    mf = tryCatch(
      stats::model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  responseVariable = if (is.data.frame(mf) && ncol(mf) > 0) {
    names(mf)[[1]]
  } else {
    all.vars(stats::formula(model))[[1]]
  }

  researchQuestion = attr(model, "wmfm_research_question", exact = TRUE) %||% ""
  researchQuestion = trimws(as.character(researchQuestion)[[1]])
  if (!nzchar(researchQuestion)) {
    researchQuestion = "(not supplied)"
  }

  adjustmentText = paste(roleMetadata$adjustmentPredictors, collapse = ", ")
  primaryText = if (length(roleMetadata$primaryPredictors) > 0) {
    paste(roleMetadata$primaryPredictors, collapse = ", ")
  } else {
    "(none listed)"
  }

  termLabels = attr(stats::terms(model), "term.labels") %||% character(0)
  hasAdjustmentInteractions = any(vapply(
    as.character(termLabels),
    termInvolvesAdjustmentVariable,
    logical(1),
    adjustmentVariables = roleMetadata$adjustmentPredictors
  ) & grepl(":", as.character(termLabels), fixed = TRUE))

  lines = c(
    "Deterministic adjustment-aware explanation scaffold:",
    paste0("Research question: ", researchQuestion),
    paste0("Response variable: ", responseVariable),
    paste0("Variables of scientific interest: ", primaryText),
    paste0("Adjustment variables: ", adjustmentText),
    paste0("Adjusted-comparison statement: The analysis addresses the research question for the variables of scientific interest after adjusting for ", adjustmentText, "."),
    "Allowed conclusion scope: Summarise only high-level conclusions about the variables of scientific interest using provided safe summaries."
  )

  adjustedSummary = getAdjustedPrimaryEffectSummary(model = model, mf = mf)
  if (nzchar(adjustedSummary)) {
    lines = c(lines, adjustedSummary)
  }

  if (isTRUE(hasAdjustmentInteractions)) {
    lines = c(
      lines,
      "Model-structure caveat: The fitted model includes terms involving adjustment variables, so the adjusted comparison is based on that model structure."
    )
  }

  lines = c(
    lines,
    "Forbidden content policy: Do not add new statistical findings.",
    "Forbidden content policy: Do not introduce adjustment-variable levels, fitted means, predicted values, contrasts, coefficients, ANOVA rows, or confidence intervals for adjustment variables.",
    "Forbidden content policy: Do not provide level-by-level interaction interpretation involving adjustment variables."
  )

  paste(lines, collapse = "\n")
}
