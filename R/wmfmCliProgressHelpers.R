#' Create a simple command-line stage tracker
#'
#' @param showProgress Logical. Should progress messages be shown?
#' @param showTiming Logical. Should timing summaries be shown?
#' @param taskLabel Character scalar naming the task.
#'
#' @return A list used internally for command-line progress and timing.
#' @keywords internal
newWmfmCliStageTracker = function(
    showProgress = TRUE,
    showTiming = TRUE,
    taskLabel = "Task"
) {

  tracker = list(
    showProgress = isTRUE(showProgress),
    showTiming = isTRUE(showTiming),
    taskLabel = safeWmfmScalar(taskLabel, naString = "Task"),
    startedAt = Sys.time(),
    stageSeconds = numeric(0),
    stageLabels = character(0)
  )

  if (tracker$showProgress) {
    cat("\n")
    cat(tracker$taskLabel, "started\n")
  }

  tracker
}

#' Run one tracked command-line stage
#'
#' @param cliTracker A tracker created by `newWmfmCliStageTracker()`.
#' @param stageLabel Character scalar describing the stage.
#' @param code Function with no arguments to execute.
#'
#' @return The return value of `code()`.
#' @keywords internal
runWmfmCliStage = function(
    cliTracker,
    stageLabel,
    code
) {

  if (!is.list(cliTracker)) {
    stop("`cliTracker` must be a tracker list.", call. = FALSE)
  }

  if (!is.function(code)) {
    stop("`code` must be a function.", call. = FALSE)
  }

  stageLabel = safeWmfmScalar(stageLabel, naString = "Stage")

  if (isTRUE(cliTracker$showProgress)) {
    cat("- ", stageLabel, "...\n", sep = "")
  }

  startedAt = Sys.time()
  out = code()
  elapsedSeconds = as.numeric(difftime(Sys.time(), startedAt, units = "secs"))

  cliTracker$stageLabels = c(cliTracker$stageLabels, stageLabel)
  cliTracker$stageSeconds = c(cliTracker$stageSeconds, elapsedSeconds)

  if (isTRUE(cliTracker$showTiming)) {
    cat("  ", stageLabel, ": ", format(round(elapsedSeconds, 2), nsmall = 2), " seconds\n", sep = "")
  }

  assign("cliTracker", cliTracker, envir = parent.frame())
  out
}

#' Finish a simple command-line stage tracker
#'
#' @param cliTracker A tracker created by `newWmfmCliStageTracker()`.
#'
#' @return Invisibly returns a list with total timing information.
#' @keywords internal
finishWmfmCliStageTracker = function(cliTracker) {

  if (!is.list(cliTracker)) {
    return(invisible(NULL))
  }

  finishedAt = Sys.time()
  elapsedSeconds = as.numeric(difftime(finishedAt, cliTracker$startedAt, units = "secs"))

  if (isTRUE(cliTracker$showTiming)) {
    cat(
      cliTracker$taskLabel,
      "finished in ",
      format(round(elapsedSeconds, 2), nsmall = 2),
      " seconds\n",
      sep = ""
    )
  } else if (isTRUE(cliTracker$showProgress)) {
    cat(cliTracker$taskLabel, "finished\n")
  }

  invisible(list(
    startedAt = cliTracker$startedAt,
    finishedAt = finishedAt,
    elapsedSeconds = elapsedSeconds,
    stageLabels = cliTracker$stageLabels,
    stageSeconds = cliTracker$stageSeconds
  ))
}
