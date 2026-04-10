#' Build run-level paired comparison data for WMFM scores
#'
#' Creates run-level paired data for all metrics in the registry that are
#' present in two score data frames. The returned object is intended to
#' support downstream summaries and plotting.
#'
#' @param leftDf Long-form score data for the left method.
#' @param rightDf Long-form score data for the right method.
#' @param registry Metric registry.
#' @param leftMethod Name of the left method.
#' @param rightMethod Name of the right method.
#'
#' @return A data frame of run-level paired values.
#' @keywords internal
buildWmfmComparisonPairs = function(leftDf, rightDf, registry, leftMethod, rightMethod) {
  validateWmfmMetricRegistry(registry)

  merged = merge(
    leftDf,
    rightDf,
    by = "runId",
    suffixes = c(".x", ".y")
  )

  registry = registry[registry$includeInComparison, , drop = FALSE]
  out = vector("list", length = nrow(registry))
  nOut = 0

  for (i in seq_len(nrow(registry))) {
    metricName = registry$metricName[i]
    leftName = paste0(metricName, ".x")
    rightName = paste0(metricName, ".y")

    if (!(leftName %in% names(merged) && rightName %in% names(merged))) {
      next
    }

    nOut = nOut + 1
    out[[nOut]] = data.frame(
      runId = merged$runId,
      metric = metricName,
      label = registry$label[i],
      group = registry$group[i],
      metricType = registry$metricType[i],
      leftMethod = leftMethod,
      rightMethod = rightMethod,
      leftValue = merged[[leftName]],
      rightValue = merged[[rightName]],
      stringsAsFactors = FALSE
    )
  }

  out = out[seq_len(nOut)]

  if (length(out) == 0) {
    return(data.frame())
  }

  do.call(rbind, out)
}
