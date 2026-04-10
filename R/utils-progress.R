#' WMFM progress and timing helpers
#'
#' Internal helpers for tracking elapsed time, average iteration time, and
#' estimated time remaining while repeated WMFM tasks are running.
#'
#' @param seconds Numeric scalar giving elapsed seconds.
#' @return For `formatWmfmSeconds()`, a length-one character string.
#' @keywords internal
formatWmfmSeconds = function(seconds) {

  seconds = suppressWarnings(as.numeric(seconds)[1])

  if (is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    return("unknown")
  }

  if (seconds < 60) {
    return(sprintf("%.1fs", seconds))
  }

  totalSeconds = as.integer(round(seconds))
  hours = totalSeconds %/% 3600L
  minutes = (totalSeconds %% 3600L) %/% 60L
  secs = totalSeconds %% 60L

  if (hours > 0L) {
    return(sprintf("%dh %02dm %02ds", hours, minutes, secs))
  }

  sprintf("%dm %02ds", minutes, secs)
}

#' Create a WMFM progress tracker
#'
#' Internal helper that initialises progress-bar and timing state for repeated
#' WMFM work.
#'
#' @param nSteps Integer. Total number of iterations.
#' @param showProgress Logical. Should a text progress bar be shown?
#' @param label Character. Short label used in timing messages.
#' @return An environment storing tracker state.
#' @keywords internal
newWmfmProgressTracker = function(
    nSteps,
    showProgress = TRUE,
    label = "Progress"
) {

  tracker = new.env(parent = emptyenv())
  tracker$nSteps = as.integer(nSteps)
  tracker$showProgress = isTRUE(showProgress)
  tracker$label = as.character(label)[1]
  tracker$startTime = Sys.time()
  tracker$iterationSeconds = numeric(0)
  tracker$progressBar = NULL

  if (tracker$showProgress && tracker$nSteps > 1L) {
    tracker$progressBar = utils::txtProgressBar(
      min = 0,
      max = tracker$nSteps,
      initial = 0,
      style = 3
    )
  }

  tracker
}

#' Update a WMFM progress tracker
#'
#' Internal helper that records the time for one finished iteration, updates the
#' progress bar, and prints timing summaries.
#'
#' @param tracker Tracker environment created by `newWmfmProgressTracker()`.
#' @param step Integer. Completed step number.
#' @param stepSeconds Numeric. Time taken for the completed step.
#' @return The updated tracker environment, invisibly.
#' @keywords internal
updateWmfmProgressTracker = function(
    tracker,
    step,
    stepSeconds
) {

  if (!is.environment(tracker)) {
    stop("`tracker` must be a tracker environment.", call. = FALSE)
  }

  step = as.integer(step)[1]
  stepSeconds = suppressWarnings(as.numeric(stepSeconds)[1])

  if (is.na(stepSeconds) || !is.finite(stepSeconds) || stepSeconds < 0) {
    stepSeconds = NA_real_
  }

  tracker$iterationSeconds[step] = stepSeconds

  if (!is.null(tracker$progressBar)) {
    utils::setTxtProgressBar(tracker$progressBar, step)
  }

  elapsedSeconds = as.numeric(difftime(Sys.time(), tracker$startTime, units = "secs"))
  meanSeconds = mean(tracker$iterationSeconds[seq_len(step)], na.rm = TRUE)

  if (is.nan(meanSeconds)) {
    meanSeconds = NA_real_
  }

  remainingSteps = max(tracker$nSteps - step, 0L)
  etaSeconds = meanSeconds * remainingSteps

  message(
    sprintf(
      "%s %d/%d | last: %s | avg: %s | elapsed: %s | eta: %s",
      tracker$label,
      step,
      tracker$nSteps,
      formatWmfmSeconds(stepSeconds),
      formatWmfmSeconds(meanSeconds),
      formatWmfmSeconds(elapsedSeconds),
      formatWmfmSeconds(etaSeconds)
    )
  )

  invisible(tracker)
}

#' Finish a WMFM progress tracker
#'
#' Internal helper that closes the text progress bar and returns timing
#' summaries.
#'
#' @param tracker Tracker environment created by `newWmfmProgressTracker()`.
#' @return A list with elapsed, average, and iteration timing summaries.
#' @keywords internal
closeWmfmProgressTracker = function(tracker) {

  if (!is.environment(tracker)) {
    stop("`tracker` must be a tracker environment.", call. = FALSE)
  }

  if (!is.null(tracker$progressBar)) {
    close(tracker$progressBar)
    tracker$progressBar = NULL
  }

  elapsedSeconds = as.numeric(difftime(Sys.time(), tracker$startTime, units = "secs"))
  averageIterationSeconds = mean(tracker$iterationSeconds, na.rm = TRUE)

  if (is.nan(averageIterationSeconds)) {
    averageIterationSeconds = NA_real_
  }

  list(
    elapsedSeconds = elapsedSeconds,
    averageIterationSeconds = averageIterationSeconds,
    iterationSeconds = tracker$iterationSeconds,
    startedAt = as.character(tracker$startTime),
    finishedAt = as.character(Sys.time())
  )
}
