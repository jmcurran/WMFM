#' Print a metric comparison summary
#'
#' @param x A `metricComparisonSummary` object.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return The input object, invisibly.
#' @export
print.metricComparisonSummary = function(x, ...) {

  if (!inherits(x, "metricComparisonSummary")) {
    stop("`x` must inherit from `metricComparisonSummary`.", call. = FALSE)
  }

  cat("<metricComparisonSummary>\n")
  cat("Rows:", nrow(x), " Metrics\n")

  colsToShow = intersect(
    c(
      "label",
      "metricType",
      "meanAbsoluteDifference",
      "proportionEqual",
      "proportionAdjacent",
      "weightedKappa",
      "modalProportionDeterministic",
      "entropyDeterministic"
    ),
    names(x)
  )

  print(utils::head(as.data.frame(x)[, colsToShow, drop = FALSE], 10))

  invisible(x)
}
