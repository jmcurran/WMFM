#' Grade an explanation against a WMFM model
#'
#' Creates a `wmfmGrade` object from a prepared `wmfmModel` object and a
#' user-supplied explanation. By default the returned object is immediately
#' scored using either the deterministic or LLM WMFM grading rubric.
#'
#' @param x A `wmfmModel` object, typically created by `runModel()`.
#' @param explanation Character scalar giving the explanation to grade.
#' @param modelAnswer Optional character scalar giving a reference answer.
#' @param method Character. One of `"deterministic"`, `"llm"`, or `"both"`.
#' @param autoScore Logical. Should the returned `wmfmGrade` object be scored
#'   immediately? Defaults to `TRUE`.
#' @param scoreScale Numeric scalar giving the displayed mark scale. Defaults to
#'   `10`.
#' @param ... Additional arguments passed to `score.wmfmGrade()` when
#'   `autoScore = TRUE`.
#'
#' @return An object of class `wmfmGrade`.
#' @export
grade.wmfmModel = function(
    x,
    explanation,
    modelAnswer = NULL,
    method = c("deterministic", "llm", "both"),
    autoScore = TRUE,
    scoreScale = 10,
    ...
) {

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.character(explanation) || length(explanation) != 1 || is.na(explanation)) {
    stop("`explanation` must be a single non-missing character string.", call. = FALSE)
  }

  if (!is.null(modelAnswer) &&
      (!is.character(modelAnswer) || length(modelAnswer) != 1 || is.na(modelAnswer))) {
    stop("`modelAnswer` must be NULL or a single non-missing character string.", call. = FALSE)
  }

  if (!is.logical(autoScore) || length(autoScore) != 1 || is.na(autoScore)) {
    stop("`autoScore` must be TRUE or FALSE.", call. = FALSE)
  }

  method = match.arg(method)

  studentRecord = buildWmfmGradeRunRecord(
    x = x,
    explanation = explanation,
    runId = 1L,
    answerRole = "student"
  )

  modelAnswerRecord = NULL

  if (!is.null(modelAnswer)) {
    modelAnswerRecord = buildWmfmGradeRunRecord(
      x = x,
      explanation = modelAnswer,
      runId = 2L,
      answerRole = "modelAnswer"
    )
  }

  out = newWmfmGrade(
    x = x,
    explanation = explanation,
    modelAnswer = modelAnswer,
    scoreScale = scoreScale,
    records = list(
      student = studentRecord,
      modelAnswer = modelAnswerRecord
    )
  )

  if (isTRUE(autoScore)) {
    if (identical(method, "both")) {
      out = score(out, method = "deterministic", ...)
      out = score(out, method = "llm", ...)
    } else {
      out = score(out, method = method, ...)
    }
  }

  out
}
