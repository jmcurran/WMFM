#' Derive paired metric comparison data directly from wmfmScores
#'
#' Internal helper that reconstructs run-level paired deterministic and LLM
#' scores for a single metric when a comparison object is not supplied.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#'
#' @return A data frame with columns \code{runId}, \code{detValue}, and
#'   \code{llmValue}.
#'
#' @keywords internal
deriveMetricComparisonDataFromScores = function(scores, metric) {
  if (is.null(scores$metricsDf) || !is.data.frame(scores$metricsDf)) {
    stop("scores$metricsDf must exist and be a data frame.")
  }

  df = scores$metricsDf

  expectedCols = c("runId", "metric", "method", "value")
  missingCols = setdiff(expectedCols, names(df))
  if (length(missingCols) > 0L) {
    stop(
      "scores$metricsDf is missing required columns: ",
      paste(missingCols, collapse = ", ")
    )
  }

  metricRows = df[df$metric == metric, , drop = FALSE]
  if (nrow(metricRows) < 1L) {
    stop("Metric not found in scores$metricsDf: ", metric)
  }

  detDf = metricRows[metricRows$method == "deterministic", c("runId", "value"), drop = FALSE]
  llmDf = metricRows[metricRows$method == "llm", c("runId", "value"), drop = FALSE]

  names(detDf)[names(detDf) == "value"] = "detValue"
  names(llmDf)[names(llmDf) == "value"] = "llmValue"

  out = merge(detDf, llmDf, by = "runId", all = TRUE, sort = FALSE)
  out
}
