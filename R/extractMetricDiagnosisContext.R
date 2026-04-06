#' Extract contextual run-level information for metric diagnosis
#'
#' Internal helper for \code{diagnose()} that attempts to recover
#' explanation text and any metric-specific scoring detail columns from a
#' \code{wmfmScores} object.
#'
#' The function is intentionally permissive because the exact storage
#' structure may evolve. It looks for common columns and returns whatever
#' relevant context is available.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#'
#' @return A data frame keyed by \code{runId}, or \code{NULL} if no
#'   contextual information is available.
#'
#' @keywords internal
extractMetricDiagnosisContext = function(scores, metric) {
  candidates = list()

  if (!is.null(scores$runsDf) && is.data.frame(scores$runsDf)) {
    runsDf = scores$runsDf

    keepCols = intersect(
      c(
        "runId",
        "explanation",
        "explanationText",
        "modelExplanation",
        "detReason",
        "llmReason",
        "detJustification",
        "llmJustification"
      ),
      names(runsDf)
    )

    if (length(keepCols) > 0L && "runId" %in% keepCols) {
      candidates[[length(candidates) + 1L]] = runsDf[, keepCols, drop = FALSE]
    }
  }

  if (!is.null(scores$metricDetailsDf) && is.data.frame(scores$metricDetailsDf)) {
    detailsDf = scores$metricDetailsDf

    if (all(c("runId", "metric") %in% names(detailsDf))) {
      detailsDf = detailsDf[detailsDf$metric == metric, , drop = FALSE]
      if (nrow(detailsDf) > 0L) {
        candidates[[length(candidates) + 1L]] = detailsDf
      }
    }
  }

  if (length(candidates) < 1L) {
    return(NULL)
  }

  out = candidates[[1L]]
  if (length(candidates) > 1L) {
    for (i in 2:length(candidates)) {
      out = merge(out, candidates[[i]], by = "runId", all = TRUE, sort = FALSE)
    }
  }

  out
}
