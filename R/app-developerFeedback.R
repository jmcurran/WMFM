#' Build a developer feedback report object
#'
#' Builds the deterministic, offline report object used by Developer Mode to
#' capture sentence-level feedback about an explanation and its support map.
#'
#' @param model A fitted model object or a `wmfmModel` object.
#' @param claimMap A `wmfmExplanationClaimEvidenceMap` object, or list with a
#'   `claims` data frame.
#' @param input Shiny input object or list-like object containing developer
#'   feedback values.
#' @param explanationText Optional full explanation text. If omitted and
#'   `model` is a `wmfmModel`, `model$explanation` is used where available.
#' @param researchQuestion Optional research question. If omitted and `model` is
#'   a `wmfmModel`, `model$researchQuestion` is used where available.
#' @param data Optional data frame. If omitted and `model` is a `wmfmModel`,
#'   `model$data` is used where available.
#' @param otherIssues Optional developer note for issues that are not tied to a
#'   single sentence. This is intended for a later UI stage.
#' @param timestamp Time stamp to record in the report.
#'
#' @return A named list representing the developer feedback report.
#' @keywords internal
#' @noRd
#' @importFrom stats formula terms
buildDeveloperFeedbackReport = function(
    model,
    claimMap,
    input = list(),
    explanationText = NULL,
    researchQuestion = NULL,
    data = NULL,
    otherIssues = NULL,
    timestamp = Sys.time()
) {

  fittedModel = extractDeveloperFeedbackModel(model)
  modelFormula = extractDeveloperFeedbackFormula(model = model, fittedModel = fittedModel)
  modelData = extractDeveloperFeedbackData(model = model, data = data)

  if (is.null(explanationText) && inherits(model, "wmfmModel")) {
    explanationText = model$explanation
  }

  if (is.null(researchQuestion) && inherits(model, "wmfmModel")) {
    researchQuestion = model$researchQuestion
  }

  claimsTable = extractDeveloperFeedbackClaims(claimMap = claimMap)

  list(
    metadata = list(
      timestamp = format(as.POSIXct(timestamp), "%Y-%m-%d %H:%M:%S %z"),
      modelType = extractDeveloperFeedbackModelType(model = model, fittedModel = fittedModel),
      modelFormula = if (!is.null(modelFormula)) paste(deparse(modelFormula), collapse = "") else NULL,
      responseVariable = extractDeveloperFeedbackResponse(modelFormula),
      predictors = extractDeveloperFeedbackPredictors(modelFormula)
    ),
    context = list(
      researchQuestion = normalizeDeveloperFeedbackNullableText(researchQuestion),
      datasetSummary = buildDeveloperFeedbackDatasetSummary(modelData),
      otherIssues = normalizeDeveloperFeedbackNullableText(otherIssues)
    ),
    explanation = list(
      fullText = normalizeDeveloperFeedbackNullableText(explanationText)
    ),
    sentenceRecords = buildDeveloperFeedbackSentenceRecords(
      claimsTable = claimsTable,
      input = input
    ),
    auditLinkage = buildDeveloperFeedbackAuditLinkage(model = model)
  )
}

#' Extract the fitted model used for developer feedback metadata
#'
#' @param model A fitted model object or a `wmfmModel` object.
#'
#' @return A fitted model object or `NULL`.
#' @keywords internal
#' @noRd
extractDeveloperFeedbackModel = function(model) {

  if (inherits(model, "wmfmModel")) {
    return(model$model)
  }

  model
}

#' Extract the formula used for developer feedback metadata
#'
#' @param model A fitted model object or a `wmfmModel` object.
#' @param fittedModel Extracted fitted model object.
#'
#' @return A formula or `NULL`.
#' @keywords internal
#' @noRd
extractDeveloperFeedbackFormula = function(model, fittedModel) {

  if (inherits(model, "wmfmModel") && inherits(model$formula, "formula")) {
    return(model$formula)
  }

  tryCatch(
    stats::formula(fittedModel),
    error = function(e) {
      NULL
    }
  )
}

#' Extract model data for developer feedback metadata
#'
#' @param model A fitted model object or a `wmfmModel` object.
#' @param data Optional supplied data frame.
#'
#' @return A data frame or `NULL`.
#' @keywords internal
#' @noRd
extractDeveloperFeedbackData = function(model, data = NULL) {

  if (is.data.frame(data)) {
    return(data)
  }

  if (inherits(model, "wmfmModel") && is.data.frame(model$data)) {
    return(model$data)
  }

  NULL
}

#' Extract model type for developer feedback metadata
#'
#' @param model A fitted model object or a `wmfmModel` object.
#' @param fittedModel Extracted fitted model object.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
extractDeveloperFeedbackModelType = function(model, fittedModel) {

  if (inherits(model, "wmfmModel") && is.character(model$modelType) && length(model$modelType) == 1) {
    return(model$modelType)
  }

  if (inherits(fittedModel, "glm") && !is.null(fittedModel$family$family)) {
    return(paste0("glm-", fittedModel$family$family))
  }

  if (inherits(fittedModel, "lm")) {
    return("lm")
  }

  class(fittedModel)[[1]] %||% NA_character_
}

#' Extract response variable from a formula
#'
#' @param modelFormula A formula or `NULL`.
#'
#' @return Character scalar or `NULL`.
#' @keywords internal
#' @noRd
extractDeveloperFeedbackResponse = function(modelFormula) {

  if (!inherits(modelFormula, "formula")) {
    return(NULL)
  }

  responseIndex = attr(stats::terms(modelFormula), "response")
  if (!is.numeric(responseIndex) || length(responseIndex) != 1 || responseIndex == 0) {
    return(NULL)
  }

  allVariables = all.vars(modelFormula)
  allVariables[[responseIndex]] %||% NULL
}

#' Extract predictors from a formula
#'
#' @param modelFormula A formula or `NULL`.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
extractDeveloperFeedbackPredictors = function(modelFormula) {

  if (!inherits(modelFormula, "formula")) {
    return(character(0))
  }

  attr(stats::terms(modelFormula), "term.labels") %||% character(0)
}

#' Build a lightweight dataset summary for developer feedback
#'
#' @param data A data frame or `NULL`.
#'
#' @return A named list or `NULL`.
#' @keywords internal
#' @noRd
buildDeveloperFeedbackDatasetSummary = function(data) {

  if (!is.data.frame(data)) {
    return(NULL)
  }

  list(
    nRows = nrow(data),
    nColumns = ncol(data),
    variableNames = names(data),
    variableClasses = lapply(data, function(x) {
      class(x)[[1]] %||% NA_character_
    })
  )
}

#' Extract claims table from a claim map
#'
#' @param claimMap A claim map object.
#'
#' @return A data frame.
#' @keywords internal
#' @noRd
extractDeveloperFeedbackClaims = function(claimMap) {

  if (is.null(claimMap) || !is.data.frame(claimMap$claims)) {
    return(data.frame(
      sentenceIndex = integer(0),
      claimText = character(0),
      claimTags = I(list()),
      stringsAsFactors = FALSE
    ))
  }

  requiredColumns = c("sentenceIndex", "claimText", "claimTags")
  missingColumns = setdiff(requiredColumns, names(claimMap$claims))

  if (length(missingColumns) > 0) {
    stop(
      "`claimMap$claims` must contain sentenceIndex, claimText, and claimTags.",
      call. = FALSE
    )
  }

  claimMap$claims
}

#' Build sentence records for developer feedback
#'
#' @param claimsTable Claims data frame.
#' @param input Shiny input object or list-like object.
#'
#' @return A list of sentence records.
#' @keywords internal
#' @noRd
buildDeveloperFeedbackSentenceRecords = function(claimsTable, input = list()) {

  if (!is.data.frame(claimsTable) || nrow(claimsTable) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(claimsTable)), function(i) {
    sentenceIndex = claimsTable$sentenceIndex[[i]]
    incorrectInputId = buildDeveloperFeedbackIncorrectInputId(sentenceIndex)
    commentInputId = buildDeveloperFeedbackCommentInputId(sentenceIndex)
    isMarkedIncorrect = isTRUE(input[[incorrectInputId]])
    comment = normalizeDeveloperFeedbackNullableText(input[[commentInputId]] %||% NULL)

    list(
      sentenceId = as.integer(sentenceIndex),
      sentenceText = cleanExplanationText(claimsTable$claimText[[i]]),
      claimTags = normalizeDeveloperFeedbackClaimTags(claimsTable$claimTags[[i]]),
      isMarkedIncorrect = isMarkedIncorrect,
      userComment = comment
    )
  })
}

#' Normalize claim tags for developer feedback
#'
#' @param claimTags Claim tag value from the claim map.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
normalizeDeveloperFeedbackClaimTags = function(claimTags) {

  tags = as.character(claimTags %||% character(0))
  tags = trimws(tags)
  tags[nzchar(tags)]
}

#' Normalize optional text for developer feedback
#'
#' @param x Value to normalize.
#'
#' @return Character scalar or `NULL`.
#' @keywords internal
#' @noRd
normalizeDeveloperFeedbackNullableText = function(x) {

  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    return(NULL)
  }

  text = trimws(as.character(x[[1]]))

  if (!nzchar(text)) {
    return(NULL)
  }

  text
}

#' Build optional audit linkage metadata for developer feedback
#'
#' @param model A fitted model object or a `wmfmModel` object.
#'
#' @return A named list or `NULL`.
#' @keywords internal
#' @noRd
buildDeveloperFeedbackAuditLinkage = function(model) {

  if (!inherits(model, "wmfmModel") || is.null(model$explanationAudit)) {
    return(NULL)
  }

  list(
    available = TRUE,
    class = class(model$explanationAudit)
  )
}

#' Convert a developer feedback report to JSON
#'
#' @param report Developer feedback report object.
#'
#' @return Character scalar containing pretty JSON.
#' @keywords internal
#' @noRd
#' @importFrom jsonlite toJSON
developerFeedbackReportToJson = function(report) {

  as.character(toJSON(
    report,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  ))
}

#' Write a developer feedback report to a JSON file
#'
#' @param report Developer feedback report object.
#' @param file Path to the output JSON file.
#'
#' @return The output file path, invisibly.
#' @keywords internal
#' @noRd
writeDeveloperFeedbackReportJson = function(report, file) {

  json = developerFeedbackReportToJson(report)
  writeLines(json, con = file, useBytes = TRUE)
  invisible(file)
}
