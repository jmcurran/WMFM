#' Score a WMFM run record using a chosen method
#'
#' Dispatches scoring for a single WMFM run record using either the
#' deterministic scorer or the language-model scorer.
#'
#' @param runRecord Named list produced by `buildWmfmRunRecord()`.
#' @param scoringMethod Character. One of `"deterministic"`, `"llm"`, or
#'   `"none"`.
#' @param scoringChat Optional chat provider object used when
#'   `scoringMethod = "llm"`.
#' @param useScoringCache Logical. Passed to `scoreWmfmRunWithLlm()` when
#'   LLM scoring is requested.
#' @param verbose Logical. Passed to `scoreWmfmRunWithLlm()` when LLM scoring is
#'   requested.
#'
#' @return A scored run record.
#' @export
scoreWmfmRunRecordByMethod = function(
    runRecord,
    scoringMethod = c("deterministic", "llm", "none"),
    scoringChat = NULL,
    useScoringCache = FALSE,
    verbose = FALSE
) {

  scoringMethod = match.arg(scoringMethod)

  if (!is.list(runRecord) || is.null(names(runRecord))) {
    stop("`runRecord` must be a named list.", call. = FALSE)
  }

  if (!is.logical(useScoringCache) || length(useScoringCache) != 1 || is.na(useScoringCache)) {
    stop("`useScoringCache` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  if (identical(scoringMethod, "none")) {
    runRecord$primaryScoringMethod = "none"
    return(runRecord)
  }

  if (identical(scoringMethod, "deterministic")) {
    if (!exists("scoreWmfmRepeatedRuns", mode = "function")) {
      stop(
        "Deterministic scoring requires `scoreWmfmRepeatedRuns()` to be available.",
        call. = FALSE
      )
    }

    runsDf = as.data.frame(runRecord, stringsAsFactors = FALSE)
    scoredDf = scoreWmfmRepeatedRuns(runsDf)

    if (!is.data.frame(scoredDf) || nrow(scoredDf) != 1) {
      stop(
        "Deterministic scorer did not return a one-row data frame.",
        call. = FALSE
      )
    }

    out = as.list(scoredDf[1, , drop = FALSE])
    out$primaryScoringMethod = "deterministic"
    return(out)
  }

  if (is.null(scoringChat)) {
    stop(
      "`scoringChat` must be supplied when `scoringMethod = \"llm\"`.",
      call. = FALSE
    )
  }

  out = scoreWmfmRunWithLlm(
    runRecord = runRecord,
    chat = scoringChat,
    useCache = useScoringCache,
    verbose = verbose
  )

  out$primaryScoringMethod = "llm"
  out
}
