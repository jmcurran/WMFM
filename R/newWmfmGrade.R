#' Create a WMFM grade object
#'
#' Creates a classed `wmfmGrade` object that stores a candidate explanation,
#' an optional reference answer, and the model context needed for scoring.
#'
#' @param x A `wmfmModel` object.
#' @param explanation Character scalar. The explanation being graded.
#' @param modelAnswer Optional character scalar giving a reference answer.
#' @param scoreScale Numeric scalar giving the displayed mark scale. Defaults to
#'   `10`.
#' @param records Optional named list of run records.
#' @param scores Optional named list of scored data frames.
#' @param feedback Optional named list of feedback components.
#' @param meta Optional named list of metadata.
#'
#' @return An object of class `wmfmGrade`.
#' @export
newWmfmGrade = function(
    x,
    explanation,
    modelAnswer = NULL,
    scoreScale = 10,
    records = list(),
    scores = list(),
    feedback = list(),
    meta = list()
) {

  if (!inherits(x, "wmfmModel")) {
    stop("`x` must inherit from `wmfmModel`.", call. = FALSE)
  }

  if (!is.character(explanation) || length(explanation) != 1 || is.na(explanation)) {
    stop("`explanation` must be a single non-missing character string.", call. = FALSE)
  }

  if (!is.null(modelAnswer)) {
    if (!is.character(modelAnswer) || length(modelAnswer) != 1 || is.na(modelAnswer)) {
      stop(
        "`modelAnswer` must be NULL or a single non-missing character string.",
        call. = FALSE
      )
    }
  }

  if (!is.numeric(scoreScale) || length(scoreScale) != 1 || is.na(scoreScale) || scoreScale <= 0) {
    stop("`scoreScale` must be a single positive number.", call. = FALSE)
  }

  if (!is.list(records)) {
    stop("`records` must be a list.", call. = FALSE)
  }

  if (!is.list(scores)) {
    stop("`scores` must be a list.", call. = FALSE)
  }

  if (!is.list(feedback)) {
    stop("`feedback` must be a list.", call. = FALSE)
  }

  if (!is.list(meta)) {
    stop("`meta` must be a list.", call. = FALSE)
  }

  out = list(
    model = x,
    input = list(
      explanation = explanation,
      modelAnswer = modelAnswer
    ),
    scoreScale = as.numeric(scoreScale),
    records = utils::modifyList(
      list(
        student = NULL,
        modelAnswer = NULL
      ),
      records
    ),
    scores = utils::modifyList(
      list(
        student = NULL,
        modelAnswer = NULL,
        metricSummary = NULL,
        mark = NA_real_,
        overallScore = NA_real_
      ),
      scores
    ),
    feedback = utils::modifyList(
      list(
        whereMarksLost = NULL,
        strengths = NULL,
        weaknesses = NULL,
        missingElements = NULL,
        modelAnswerComparison = NULL
      ),
      feedback
    ),
    meta = utils::modifyList(
      list(
        createdAt = as.character(Sys.time()),
        sourceClass = class(x)[1],
        scored = FALSE
      ),
      meta
    )
  )

  class(out) = c("wmfmGrade", "list")
  out
}
