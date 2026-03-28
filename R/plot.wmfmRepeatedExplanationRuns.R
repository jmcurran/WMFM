#' Plot method for repeated WMFM explanation runs
#'
#' Convenience plot method for repeated WMFM explanation results. By default it
#' produces a heatmap of extracted claim fields using
#' `plotWmfmExplanationClaimHeatmap()`, but it can also be directed to plot the
#' judged quality fields or aggregate score fields from the revised repeated-run
#' schema.
#'
#' The method accepts either:
#' \itemize{
#'   \item an object of class `wmfmRepeatedExplanationRuns`, expected to contain
#'   a `runsDf` element, or
#'   \item a plain list with a `runsDf` element.
#' }
#'
#' @param x A repeated-run object or list containing `runsDf`.
#' @param fieldColumns Optional character vector of field columns to plot.
#' @param plotType Character. One of `"claims"`, `"judged"`, `"scores"`, or
#'   `"auto"`.
#' @param ... Additional arguments passed to
#'   `plotWmfmExplanationClaimHeatmap()`, including `sortColumns = "purity"`
#'   when you want columns ordered by purity.
#'
#' @return Invisibly returns the object returned by
#'   `plotWmfmExplanationClaimHeatmap()`.
#' @examples
#' \dontrun{
#' plot(repeatedRuns)
#' plot(repeatedRuns, plotType = "judged")
#' plot(repeatedRuns, sortColumns = "purity")
#' }
#' @export
plot.wmfmRepeatedExplanationRuns = function(
    x,
    fieldColumns = NULL,
    plotType = c("claims", "judged", "scores", "auto"),
    ...
) {
  plotType = match.arg(plotType)

  if (is.data.frame(x)) {
    runsDf = x
  } else if (is.list(x) && "runsDf" %in% names(x) && is.data.frame(x$runsDf)) {
    runsDf = x$runsDf
  } else {
    stop(
      "`x` must be a data.frame or a list containing a data.frame named `runsDf`.",
      call. = FALSE
    )
  }

  plotWmfmExplanationClaimHeatmap(
    runsDf = runsDf,
    fieldColumns = fieldColumns,
    plotType = plotType,
    ...
  )
}
