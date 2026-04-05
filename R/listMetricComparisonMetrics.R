#' List valid metric names for a score comparison object
#'
#' Returns the metric names available from the metric registry stored on a
#' `wmfmScoreComparison` object.
#'
#' This helper is mainly intended to make dynamic metric selection easier in
#' interactive use, since IDE tab completion does not automatically expose
#' choices that are supplied to `match.arg()` dynamically.
#'
#' @param x A `wmfmScoreComparison` object.
#'
#' @return A character vector of valid metric names.
#' @export
listMetricComparisonMetrics = function(x) {

  if (!inherits(x, "wmfmScoreComparison")) {
    stop("`x` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  registry = x$registry

  if (is.null(registry) || !is.data.frame(registry) || nrow(registry) == 0) {
    stop("`x$registry` must be a non-empty data.frame.", call. = FALSE)
  }

  as.character(registry$metricName)
}
