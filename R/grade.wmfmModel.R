#' Grade an explanation against a WMFM model
#'
#' Creates a `wmfmGrade` object from a prepared `wmfmModel` object and a
#' user-supplied explanation. By default the returned object is immediately
#' scored using the existing deterministic WMFM rubric.
#'
#' @param x A `wmfmModel` object, typically created by `runModel()`.
#' @param explanation Character scalar giving the explanation to grade.
#' @param modelAnswer Optional character scalar giving a reference answer.
#' @param score Logical. Should the returned `wmfmGrade` object be scored
#'   immediately? Defaults to `TRUE`.
#' @param scoreScale Numeric scalar giving the displayed mark scale. Defaults to
#'   `10`.
#' @param ... Additional arguments passed to `score.wmfmGrade()`.
#'
#' @return An object of class `wmfmGrade`.
#' @export
grade.wmfmModel = function(
    x,
    explanation,
    modelAnswer = NULL,
    score = TRUE,
    scoreScale = 10,
    ...
) {

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.character(explanation) || length(explanation) != 1 || is.na(explanation)) {
    stop("`explanation` must be a single non-missing character string.", call. = FALSE)
  }

  if (!is.logical(score) || length(score) != 1 || is.na(score)) {
    stop("`score` must be TRUE or FALSE.", call. = FALSE)
  }

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

  if (isTRUE(score)) {
    out = score(out, ...)
  }

  out
}
