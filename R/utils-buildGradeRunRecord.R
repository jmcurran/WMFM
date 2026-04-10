#' Build a single run record for WMFM grading
#'
#' Internal helper that converts a `wmfmModel` object and a supplied
#' explanation into the same raw run-record structure used elsewhere in WMFM.
#'
#' @param x A `wmfmModel` object.
#' @param explanation Character scalar.
#' @param runId Integer run identifier.
#' @param answerRole Character scalar describing the answer role.
#'
#' @return A named list containing one raw run record.
#'
#' @keywords internal
#' @noRd
buildWmfmGradeRunRecord = function(
    x,
    explanation,
    runId = 1L,
    answerRole = c("student", "modelAnswer")
) {
  answerRole = match.arg(answerRole)

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.character(explanation) || length(explanation) != 1 || is.na(explanation)) {
    stop("`explanation` must be a single non-missing character string.", call. = FALSE)
  }

  exampleName = x$meta$exampleName %||% NA_character_
  packageName = x$meta$package %||% NA_character_

  out = buildWmfmRunRecord(
    runId = as.integer(runId),
    exampleName = exampleName,
    package = packageName,
    modelType = x$modelType,
    formula = paste(deparse(x$formula), collapse = " "),
    equationsText = extractWmfmText(x$equations),
    explanationText = explanation,
    errorMessage = NA_character_,
    interactionTerms = x$interactionTerms %||% character(0),
    interactionMinPValue = x$interactionMinPValue %||% NA_real_,
    interactionAlpha = x$meta$interactionAlpha %||% 0.05
  )

  out$answerRole = answerRole
  out
}
