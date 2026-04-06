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

  detNonMissing = metricDf$detValue[!is.na(metricDf$detValue)]
  llmNonMissing = metricDf$llmValue[!is.na(metricDf$llmValue)]

  detUnique = sort(unique(detNonMissing))
  llmUnique = sort(unique(llmNonMissing))

  safeMean = function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    mean(x, na.rm = TRUE)
  }

  safeMedian = function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    stats::median(x, na.rm = TRUE)
  }

  safeMin = function(x) {
    if (length(x) < 1L) {
      return(NA_real_)
    }

    min(x)
  }

  safeMax = function(x) {
    if (length(x) < 1L) {
      return(NA_real_)
    }

    max(x)
  }

  out = data.frame(
    metric = metric,
    nRuns = nRuns,
    nDisagree = nDisagree,
    disagreementRate = if (nRuns > 0L) nDisagree / nRuns else NA_real_,
    meanDet = safeMean(metricDf$detValue),
    meanLlm = safeMean(metricDf$llmValue),
    meanLlmMinusDet = safeMean(metricDf$llmMinusDet),
    medianLlmMinusDet = safeMedian(metricDf$llmMinusDet),
    llmHigherCount = llmHigherCount,
    detHigherCount = detHigherCount,
    directionConsistency = max(llmHigherCount, detHigherCount) / max(1, nDisagree),
    detConstant = length(detUnique) == 1L && length(detUnique) > 0L,
    llmConstant = length(llmUnique) == 1L && length(llmUnique) > 0L,
    detMin = safeMin(detNonMissing),
    detMax = safeMax(detNonMissing),
    llmMin = safeMin(llmNonMissing),
    llmMax = safeMax(llmNonMissing),
    stringsAsFactors = FALSE
  )

  out
}
