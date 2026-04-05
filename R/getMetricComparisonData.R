#' Extract run-level comparison data for a metric
#'
#' Returns run-level paired scores for a given metric from a
#' `wmfmScoreComparison` object.
#'
#' This accessor uses the metric registry stored on the comparison object as the
#' source of truth for valid metric names. Because the choices are dynamic,
#' `match.arg()` is used for validation and partial matching, but IDE tab
#' completion will not show the possible values automatically. Use
#' `listMetricComparisonMetrics()` to inspect the available metric names.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param metric Character name of the metric. Must be one of the metric names
#'   returned by `listMetricComparisonMetrics(x)`.
#'
#' @return An object of class `metricComparisonData` with one row per run.
#'   When one of the compared methods is deterministic, the returned data frame
#'   includes a `detValue` column and a method-specific value column such as
#'   `llmValue`. When the comparison is between deterministic and LLM scoring,
#'   the output also includes `llmMinusDet` and `detMinusLlm`.
#' @export
getMetricComparisonData = function(x, metric) {

  if (!inherits(x, "wmfmScoreComparison")) {
    stop("`x` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  metricChoices = listMetricComparisonMetrics(x)

  if (missing(metric)) {
    stop(
      "`metric` must be supplied. Use `listMetricComparisonMetrics(x)` to see valid choices.",
      call. = FALSE
    )
  }

  metric = match.arg(metric, choices = metricChoices)

  df = x$pairData

  if (is.null(df) || nrow(df) == 0) {
    stop("`pairData` is empty.", call. = FALSE)
  }

  subDf = df[df$metric == metric, c(
    "runId",
    "leftValue",
    "rightValue"
  ), drop = FALSE]

  if (nrow(subDf) == 0) {
    stop("No data found for metric: ", metric, call. = FALSE)
  }

  leftMethod = as.character(x$leftMethod)[1]
  rightMethod = as.character(x$rightMethod)[1]

  if (identical(leftMethod, "deterministic")) {
    detVals = subDf$leftValue
    otherVals = subDf$rightValue
    otherName = rightMethod
  } else if (identical(rightMethod, "deterministic")) {
    detVals = subDf$rightValue
    otherVals = subDf$leftValue
    otherName = leftMethod
  } else {
    detVals = subDf$leftValue
    otherVals = subDf$rightValue
    otherName = rightMethod
  }

  out = data.frame(
    runId = subDf$runId,
    detValue = detVals,
    stringsAsFactors = FALSE
  )

  otherColName = paste0(otherName, "Value")
  out[[otherColName]] = otherVals

  if (identical(otherName, "llm")) {
    out$llmMinusDet = otherVals - detVals
    out$detMinusLlm = detVals - otherVals
    diffVals = out$llmMinusDet
  } else {
    out$difference = otherVals - detVals
    diffVals = out$difference
  }

  out$absDifference = abs(diffVals)

  out$agreementClass = ifelse(
    out$absDifference == 0,
    "exact",
    ifelse(out$absDifference == 1, "adjacent", "different")
  )

  rownames(out) = NULL

  structure(
    out,
    metric = metric,
    leftMethod = leftMethod,
    rightMethod = rightMethod,
    class = c("metricComparisonData", "data.frame")
  )
}
