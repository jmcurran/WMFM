#' Build per-run text and timing metric data for a WMFM runs object
#'
#' Extracts selected per-run descriptive metrics from a `wmfmRuns` object for
#' plotting and inspection.
#'
#' @param x A `wmfmRuns` object.
#'
#' @return A data frame with one row per run.
#' @export
getWmfmRunsTextMetricsData = function(x) {
  if (!inherits(x, "wmfmRuns")) {
    stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  runsDf = do.call(
    rbind,
    lapply(
      x$runs,
      function(run) {
        as.data.frame(run, stringsAsFactors = FALSE)
      }
    )
  )

  rownames(runsDf) = NULL

  requiredFields = c(
    "runId",
    "wordCount",
    "sentenceCount"
  )

  missingFields = setdiff(requiredFields, names(runsDf))
  if (length(missingFields) > 0) {
    stop(
      "Missing expected text metric fields in `wmfmRuns`: ",
      paste(missingFields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!("runElapsedSeconds" %in% names(runsDf))) {
    runsDf$runElapsedSeconds = NA_real_
  }

  out = runsDf[, c(
    "runId",
    "wordCount",
    "sentenceCount",
    "runElapsedSeconds"
  )]

  out$runId = as.integer(out$runId)
  out$wordCount = as.numeric(out$wordCount)
  out$sentenceCount = as.numeric(out$sentenceCount)
  out$runElapsedSeconds = as.numeric(out$runElapsedSeconds)

  out
}
