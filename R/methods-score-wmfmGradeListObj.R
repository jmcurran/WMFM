#' Score a WMFM grade list object
#'
#' Scores each contained `wmfmGrade` object and records batch-level timing.
#'
#' @param x A `wmfmGradeListObj` object.
#' @param method Character. One of `"deterministic"`, `"llm"`, or `"both"`.
#' @param nLlm Integer. Number of repeated LLM gradings per explanation.
#' @param confirmLargeLlmJob Logical. Whether to allow large requests.
#' @param maxLlmJobsWithoutConfirmation Integer. Maximum number of LLM calls
#'   allowed without explicit confirmation.
#' @param showProgress Logical. Should progress messages be shown?
#' @param ... Additional arguments passed to `score.wmfmGrade()`.
#'
#' @return A scored `wmfmGradeListObj` object.
#' @export
score.wmfmGradeListObj = function(
    x,
    method = c("deterministic", "llm", "both"),
    nLlm = 1L,
    confirmLargeLlmJob = FALSE,
    maxLlmJobsWithoutConfirmation = 20L,
    showProgress = TRUE,
    ...
) {

  if (!inherits(x, "wmfmGradeListObj")) {
    stop("`x` must inherit from `wmfmGradeListObj`.", call. = FALSE)
  }

  method = match.arg(method)
  nLlm = as.integer(nLlm)[1]

  if (is.na(nLlm) || nLlm < 1L) {
    stop("`nLlm` must be an integer greater than or equal to 1.", call. = FALSE)
  }

  totalLlmCalls = computeWmfmLlmJobCount(
    nExplanations = length(x$grades),
    method = method,
    nLlm = nLlm
  )

  enforceWmfmLlmJobGuard(
    totalLlmCalls = totalLlmCalls,
    nExplanations = length(x$grades),
    nLlm = nLlm,
    confirmLargeLlmJob = confirmLargeLlmJob,
    maxLlmJobsWithoutConfirmation = maxLlmJobsWithoutConfirmation
  )

  tracker = newWmfmProgressTracker(
    nSteps = length(x$grades),
    showProgress = showProgress,
    label = "Grading explanations"
  )

  for (i in seq_along(x$grades)) {
    startedAt = Sys.time()

    if (identical(method, "both")) {
      x$grades[[i]] = score(
        x$grades[[i]],
        method = "deterministic",
        showProgress = FALSE,
        ...
      )
      x$grades[[i]] = score(
        x$grades[[i]],
        method = "llm",
        nLlm = nLlm,
        showProgress = FALSE,
        ...
      )
    } else {
      x$grades[[i]] = score(
        x$grades[[i]],
        method = method,
        nLlm = nLlm,
        showProgress = FALSE,
        ...
      )
    }

    stepSeconds = as.numeric(difftime(Sys.time(), startedAt, units = "secs"))
    updateWmfmProgressTracker(tracker, i, stepSeconds)
  }

  timing = closeWmfmProgressTracker(tracker)

  scoredMethods = unique(unlist(lapply(x$grades, function(gradeObj) {
    gradeObj$meta$scoredMethods %||% character(0)
  })))

  x$meta$method = method
  x$meta$nLlm = nLlm
  x$meta$totalLlmCalls = totalLlmCalls
  x$meta$elapsedSeconds = timing$elapsedSeconds
  x$meta$meanSecondsPerExplanation = timing$averageIterationSeconds
  x$meta$meanSecondsPerLlmCall = if (totalLlmCalls > 0L) {
    timing$elapsedSeconds / totalLlmCalls
  } else {
    NA_real_
  }
  x$meta$confirmLargeLlmJob = confirmLargeLlmJob
  x$meta$maxLlmJobsWithoutConfirmation = maxLlmJobsWithoutConfirmation
  x$meta$scored = TRUE
  x$meta$scoredMethods = scoredMethods
  x$meta$lastScoredMethod = if (identical(method, "both")) "llm" else method

  x
}
