#' Construct a wmfmGradeComparison object
#'
#' @param summary Named list containing top-level comparison summaries.
#' @param metricComparison Data frame of metric-by-metric comparisons.
#' @param advisoryComparison Named list of advisory flags by method.
#' @param comparedMethods Character vector of compared methods.
#' @param sourceGrade Original `wmfmGrade` object.
#'
#' @return An object of class `wmfmGradeComparison`.
#' @keywords internal
#' @noRd
newWmfmGradeComparison = function(
    summary,
    metricComparison,
    advisoryComparison,
    comparedMethods,
    sourceGrade
) {

  out = list(
    summary = summary,
    metricComparison = metricComparison,
    advisoryComparison = advisoryComparison,
    comparedMethods = comparedMethods,
    sourceGrade = sourceGrade
  )

  class(out) = "wmfmGradeComparison"
  out
}
