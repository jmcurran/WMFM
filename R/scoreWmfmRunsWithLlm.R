#' Score multiple WMFM run records using a language model
#'
#' Applies `scoreWmfmRunWithLlm()` to one or more WMFM run records. This helper
#' is designed for repeated-run workflows where the number of explanations to
#' score can vary from 1 to `N`.
#'
#' @param runRecords Either a single run record, or a list of run records, where
#'   each run record is a named list produced by `buildWmfmRunRecord()`.
#' @param chat A chat provider object as returned by `getChatProvider()`.
#' @param useCache Logical. Should scoring results be cached and reused for
#'   identical run records? Defaults to `FALSE`.
#' @param showProgress Logical. Should a text progress bar be shown when scoring
#'   multiple runs interactively? Defaults to `TRUE`.
#' @param verbose Logical. Should raw scoring responses be printed? Defaults to
#'   `FALSE`.
#'
#' @return If the input is a single run record, returns one scored run record.
#'   Otherwise returns a list of scored run records of the same length as the
#'   input list.
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

  if (is.list(runRecords) && !is.null(names(runRecords)) && "explanationText" %in% names(runRecords)) {
    return(
      scoreWmfmRunWithLlm(
        runRecord = runRecords,
        chat = chat,
        useCache = useCache,
        verbose = verbose
      )
    )
  }

  if (!is.list(runRecords) || length(runRecords) == 0) {
    stop(
      "`runRecords` must be either a single run record or a non-empty list of run records.",
      call. = FALSE
    )
  }

  badIndex = which(!vapply(runRecords, function(x) is.list(x) && !is.null(names(x)), logical(1)))
  if (length(badIndex) > 0) {
    stop("All elements of `runRecords` must be named lists.", call. = FALSE)
  }

  if (!is.logical(showProgress) || length(showProgress) != 1 || is.na(showProgress)) {
    stop("`showProgress` must be TRUE or FALSE.", call. = FALSE)
  }

  nRuns = length(runRecords)
  out = vector("list", nRuns)

  pb = NULL
  usePb = isTRUE(showProgress) && interactive() && nRuns > 1

  if (usePb) {
    pb = utils::txtProgressBar(min = 0, max = nRuns, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in seq_len(nRuns)) {
    out[[i]] = scoreWmfmRunWithLlm(
      runRecord = runRecords[[i]],
      chat = chat,
      useCache = useCache,
      verbose = verbose
    )

    if (usePb) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  names(out) = names(runRecords)
  out
}
