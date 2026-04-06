#' Score multiple WMFM run records using a language model
#'
#' Applies `scoreWmfmRunWithLlm()` to one or more WMFM run records. This helper
#' is designed for repeated-run workflows where the number of explanations to
#' score can vary from 1 to `N`.
#'
#' @param runRecords Either a single run record, a list of run records, or a
#'   repeated-run object containing a `runsDf` component.
#' @param chat A chat provider object as returned by `getChatProvider()`.
#' @param useCache Logical. Should scoring results be cached and reused for
#'   identical run records? Defaults to `FALSE`.
#' @param showProgress Logical. Should a text progress bar and timing summary be
#'   shown while scoring repeated runs? Defaults to `TRUE`.
#' @param verbose Logical. Should raw scoring responses be printed? Defaults to
#'   `FALSE`.
#'
#' @return If the input is a single run record, returns one scored run record.
#'   If the input is a repeated-run object, returns that object with its `runsDf`
#'   replaced by the scored runs and with `primaryScoringMethod = "llm"`.
#'   Otherwise returns a list of scored run records of the same length as the
#'   input list. Repeated-run outputs also receive timing metadata.
#' @export
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
scoreWmfmRunsWithLlm = function(
    runRecords,
    chat,
    useCache = FALSE,
    showProgress = TRUE,
    verbose = FALSE
) {

  if (!is.logical(showProgress) || length(showProgress) != 1 || is.na(showProgress)) {
    stop("`showProgress` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.list(runRecords) && !is.null(names(runRecords)) && "explanationText" %in% names(runRecords)) {
    iterationStartTime = Sys.time()

    scoredRun = scoreWmfmRunWithLlm(
      runRecord = runRecords,
      chat = chat,
      useCache = useCache,
      verbose = verbose
    )

    scoredRun$llmScoringElapsedSeconds = as.numeric(
      difftime(Sys.time(), iterationStartTime, units = "secs")
    )

    return(scoredRun)
  }

  if (is.list(runRecords) && !is.null(runRecords$runsDf) && is.data.frame(runRecords$runsDf)) {
    runList = lapply(seq_len(nrow(runRecords$runsDf)), function(i) {
      as.list(runRecords$runsDf[i, , drop = FALSE])
    })

    scoredList = scoreWmfmRunsWithLlm(
      runRecords = runList,
      chat = chat,
      useCache = useCache,
      showProgress = showProgress,
      verbose = verbose
    )

    scoredDf = do.call(rbind, lapply(scoredList, as.data.frame))
    rownames(scoredDf) = NULL

    scoringSeconds = vapply(
      scoredList,
      function(run) {
        value = suppressWarnings(as.numeric(run$llmScoringElapsedSeconds)[1])
        if (length(value) == 0 || is.na(value) || !is.finite(value)) {
          return(NA_real_)
        }
        value
      },
      numeric(1)
    )

    runRecords$runsDf = scoredDf
    runRecords$primaryScoringMethod = "llm"
    runRecords$meta$llmScoringElapsedSeconds = sum(scoringSeconds, na.rm = TRUE)
    runRecords$meta$llmScoringAverageSeconds = mean(scoringSeconds, na.rm = TRUE)
    runRecords$meta$llmScoringRunSeconds = scoringSeconds
    return(runRecords)
  }

  if (!is.list(runRecords) || length(runRecords) == 0) {
    stop(
      "`runRecords` must be either a single run record, a non-empty list of run records, or a repeated-run object with `runsDf`.",
      call. = FALSE
    )
  }

  badIndex = which(!vapply(runRecords, function(x) is.list(x) && !is.null(names(x)), logical(1)))
  if (length(badIndex) > 0) {
    stop("All elements of `runRecords` must be named lists.", call. = FALSE)
  }

  nRuns = length(runRecords)
  out = vector("list", nRuns)
  tracker = newWmfmProgressTracker(
    nSteps = nRuns,
    showProgress = showProgress,
    label = "LLM scoring"
  )
  on.exit(closeWmfmProgressTracker(tracker), add = TRUE)

  for (i in seq_len(nRuns)) {
    iterationStartTime = Sys.time()

    out[[i]] = scoreWmfmRunWithLlm(
      runRecord = runRecords[[i]],
      chat = chat,
      useCache = useCache,
      verbose = verbose
    )

    iterationSeconds = as.numeric(
      difftime(Sys.time(), iterationStartTime, units = "secs")
    )
    out[[i]]$llmScoringElapsedSeconds = iterationSeconds

    if (isTRUE(showProgress)) {
      updateWmfmProgressTracker(
        tracker = tracker,
        step = i,
        stepSeconds = iterationSeconds
      )
    }
  }

  names(out) = names(runRecords)
  timing = closeWmfmProgressTracker(tracker)
  attr(out, "timing") = timing
  out
}
