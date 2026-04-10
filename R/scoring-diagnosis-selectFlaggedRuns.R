#' Select flagged runs for metric diagnosis
#'
#' Internal helper for \code{diagnose()} that selects the most informative
#' disagreement cases for inspection.
#'
#' @param metricDf Run-level metric diagnosis data.
#' @param maxExamples Maximum number of examples to return.
#'
#' @return A data frame.
#'
#' @keywords internal
selectFlaggedMetricRuns = function(metricDf, maxExamples = 5) {
  flaggedDf = metricDf[metricDf$disagrees, , drop = FALSE]

  if (nrow(flaggedDf) < 1L) {
    return(flaggedDf)
  }

  flaggedDf$absDifference = abs(flaggedDf$llmMinusDet)
  flaggedDf = flaggedDf[order(-flaggedDf$absDifference, flaggedDf$runId), , drop = FALSE]

  utils::head(flaggedDf, maxExamples)
}
