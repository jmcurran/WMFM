#' Build summary statistics for metric diagnosis
#'
#' Internal helper for \code{diagnose()}.
#'
#' @param metricDf Run-level metric diagnosis data.
#' @param metric Metric name.
#'
#' @return A one-row data frame.
#'
#' @keywords internal
buildMetricDiagnosisSummary = function(metricDf, metric) {
  nRuns = nrow(metricDf)
  nDisagree = sum(metricDf$disagrees, na.rm = TRUE)

  llmHigherCount = sum(metricDf$disagreementDirection == "llmHigher", na.rm = TRUE)
  detHigherCount = sum(metricDf$disagreementDirection == "detHigher", na.rm = TRUE)

  detUnique = sort(unique(metricDf$detValue))
  llmUnique = sort(unique(metricDf$llmValue))

  out = data.frame(
    metric = metric,
    nRuns = nRuns,
    nDisagree = nDisagree,
    disagreementRate = nDisagree / nRuns,
    meanDet = mean(metricDf$detValue, na.rm = TRUE),
    meanLlm = mean(metricDf$llmValue, na.rm = TRUE),
    meanLlmMinusDet = mean(metricDf$llmMinusDet, na.rm = TRUE),
    medianLlmMinusDet = stats::median(metricDf$llmMinusDet, na.rm = TRUE),
    llmHigherCount = llmHigherCount,
    detHigherCount = detHigherCount,
    directionConsistency = max(llmHigherCount, detHigherCount) / max(1, nDisagree),
    detConstant = length(detUnique) == 1L,
    llmConstant = length(llmUnique) == 1L,
    detMin = min(metricDf$detValue, na.rm = TRUE),
    detMax = max(metricDf$detValue, na.rm = TRUE),
    llmMin = min(metricDf$llmValue, na.rm = TRUE),
    llmMax = max(metricDf$llmValue, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  out
}
