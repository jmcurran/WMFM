#' Score a WMFM grade object
#'
#' Scores a `wmfmGrade` object using the existing deterministic WMFM scoring
#' rubric. The candidate explanation is always scored. If a reference answer is
#' present, it is scored separately and used to enrich feedback.
#'
#' @param x A `wmfmGrade` object.
#' @param preferredMinWords Integer. Passed to `scoreWmfmRepeatedRuns()`.
#' @param preferredMaxWords Integer. Passed to `scoreWmfmRepeatedRuns()`.
#' @param fatalFlawCap Numeric. Passed to `scoreWmfmRepeatedRuns()`.
#' @param passThreshold Numeric. Passed to `scoreWmfmRepeatedRuns()`.
#' @param ... Additional arguments passed to `scoreWmfmRepeatedRuns()`.
#'
#' @return A scored `wmfmGrade` object.
#' @export
score.wmfmGrade = function(
    x,
    preferredMinWords = 80L,
    preferredMaxWords = 220L,
    fatalFlawCap = 40,
    passThreshold = 65,
    ...
) {

  if (!inherits(x, "wmfmGrade")) {
    stop("`x` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  if (is.null(x$records$student)) {
    stop("`x` does not contain a student run record.", call. = FALSE)
  }

  scoreOneRecord = function(record) {
    df = as.data.frame(record, stringsAsFactors = FALSE)
    scored = scoreWmfmRepeatedRuns(
      runsDf = df,
      preferredMinWords = preferredMinWords,
      preferredMaxWords = preferredMaxWords,
      penaliseDuplicates = FALSE,
      fatalFlawCap = fatalFlawCap,
      passThreshold = passThreshold,
      ...
    )

    rownames(scored) = NULL
    scored
  }

  studentScoreDf = scoreOneRecord(x$records$student)
  modelAnswerScoreDf = NULL

  if (!is.null(x$records$modelAnswer)) {
    modelAnswerScoreDf = scoreOneRecord(x$records$modelAnswer)
  }

  feedback = summariseWmfmGradeLosses(
    studentScoreDf = studentScoreDf,
    modelAnswerScoreDf = modelAnswerScoreDf
  )

  overallScore = suppressWarnings(as.numeric(studentScoreDf$overallScore[1]))
  mark = overallScore / 100 * x$scoreScale

  x$scores$student = studentScoreDf
  x$scores$modelAnswer = modelAnswerScoreDf
  x$scores$metricSummary = feedback$metricSummary
  x$scores$overallScore = overallScore
  x$scores$mark = mark

  x$feedback$whereMarksLost = feedback$whereMarksLost
  x$feedback$strengths = feedback$strengths
  x$feedback$modelAnswerComparison = feedback$modelAnswerComparison

  x$meta$scored = TRUE
  x$meta$scoredAt = as.character(Sys.time())
  x$meta$preferredMinWords = preferredMinWords
  x$meta$preferredMaxWords = preferredMaxWords
  x$meta$fatalFlawCap = fatalFlawCap
  x$meta$passThreshold = passThreshold

  x
}
