#' Score multiple WMFM runs using an LLM
#'
#' Scores each raw run record in a `wmfmRuns` object using an LLM-based scorer.
#' The returned value is a list of score records containing only judged fields
#' and score summaries. The input run records are not modified.
#'
#' Timing metadata is attached to the returned list as an attribute named
#' `"timing"`.
#'
#' @param runRecords List of raw WMFM run records.
#' @param chat Chat provider object used for LLM scoring.
#' @param useCache Logical. Whether to allow cached LLM responses.
#' @param showProgress Logical. Whether to display progress and timing
#'   information during scoring.
#' @param verbose Logical. Whether to print additional diagnostic information.
#'
#' @return A list of score records. Each element corresponds to one input run
#'   record and contains only judged fields and score summaries. A `"timing"`
#'   attribute is attached to the returned list.
#' @export
scoreWmfmRunsWithLlm = function(
    runRecords,
    chat,
    useCache = FALSE,
    showProgress = TRUE,
    verbose = FALSE
) {
  if (!is.list(runRecords) || length(runRecords) == 0) {
    stop("`runRecords` must be a non-empty list of raw run records.", call. = FALSE)
  }

  if (missing(chat) || is.null(chat)) {
    stop("`chat` must be supplied for LLM scoring.", call. = FALSE)
  }

  nRuns = length(runRecords)
  scoredRuns = vector("list", nRuns)
  iterationSeconds = numeric(nRuns)

  startedAt = Sys.time()

  progressBar = NULL
  if (isTRUE(showProgress)) {
    progressBar = utils::txtProgressBar(
      min = 0,
      max = nRuns,
      initial = 0,
      style = 3
    )

    on.exit(
      close(progressBar),
      add = TRUE
    )
  }

  for (i in seq_len(nRuns)) {
    iterationStart = Sys.time()

    scoredRuns[[i]] = scoreWmfmRunWithLlm(
      runRecord = runRecords[[i]],
      chat = chat,
      useCache = useCache,
      verbose = verbose
    )

    if (!is.list(scoredRuns[[i]]) || length(scoredRuns[[i]]) == 0) {
      stop(
        sprintf(
          "LLM scoring failed to return a valid score record for run %d.",
          i
        ),
        call. = FALSE
      )
    }

    iterationSeconds[i] = as.numeric(
      difftime(Sys.time(), iterationStart, units = "secs")
    )

    if (isTRUE(showProgress)) {
      utils::setTxtProgressBar(progressBar, i)

      avgSeconds = mean(iterationSeconds[seq_len(i)])
      remainingRuns = nRuns - i
      etaSeconds = avgSeconds * remainingRuns

      message(
        sprintf(
          "\nRun %d/%d completed in %.2f s | elapsed %.2f s | ETA %.2f s",
          i,
          nRuns,
          iterationSeconds[i],
          sum(iterationSeconds[seq_len(i)]),
          etaSeconds
        )
      )
    }
  }

  finishedAt = Sys.time()

  timing = list(
    startedAt = as.character(startedAt),
    finishedAt = as.character(finishedAt),
    iterationSeconds = iterationSeconds,
    averageIterationSeconds = mean(iterationSeconds),
    totalElapsedSeconds = as.numeric(
      difftime(finishedAt, startedAt, units = "secs")
    )
  )

  attr(scoredRuns, "timing") = timing

  scoredRuns
}
