#' Resolve run-level data for metric diagnosis
#'
#' Internal helper for \code{diagnose()}.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#' @param cmp Optional \code{wmfmScoreComparison} object.
#' @param runs Optional \code{wmfmRuns} object or compatible run container.
#'
#' @return A data frame with one row per run.
#'
#' @keywords internal
resolveMetricDiagnosisData = function(scores, metric, cmp = NULL, runs = NULL) {
  if (!is.null(cmp)) {
    if (!exists("getMetricComparisonData", mode = "function")) {
      stop("getMetricComparisonData() is required when cmp is supplied.")
    }

    metricDf = getMetricComparisonData(cmp, metric)
  } else {
    metricDf = deriveMetricComparisonDataFromScores(scores = scores, metric = metric)
  }

  if (!is.data.frame(metricDf) || nrow(metricDf) < 1L) {
    stop("Could not resolve run-level comparison data for metric: ", metric)
  }

  requiredCols = c("runId", "detValue", "llmValue")
  missingCols = setdiff(requiredCols, names(metricDf))
  if (length(missingCols) > 0L) {
    stop(
      "Metric comparison data is missing required columns: ",
      paste(missingCols, collapse = ", ")
    )
  }

  metricDf$detMinusLlm = metricDf$detValue - metricDf$llmValue
  metricDf$llmMinusDet = metricDf$llmValue - metricDf$detValue
  metricDf$disagrees = metricDf$detValue != metricDf$llmValue
  metricDf$disagreementDirection = ifelse(
    metricDf$llmValue > metricDf$detValue,
    "llmHigher",
    ifelse(metricDf$llmValue < metricDf$detValue, "detHigher", "same")
  )

  extraDf = extractMetricDiagnosisContext(
    scores = scores,
    metric = metric,
    runs = runs
  )

  if (!is.null(extraDf) && is.data.frame(extraDf) && "runId" %in% names(extraDf)) {
    metricDf = merge(
      metricDf,
      extraDf,
      by = "runId",
      all.x = TRUE,
      sort = FALSE
    )
  }

  metricDf[order(metricDf$runId), , drop = FALSE]
}
