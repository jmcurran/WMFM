#' Plot a WMFM runs object
#'
#' Produces plots for a raw `wmfmRuns` object. These plots are descriptive and
#' focus on run-to-run variation in generated outputs, extracted claims, and run
#' metrics. Judged fields and score summaries are intentionally excluded and
#' belong to `wmfmScores` objects instead.
#'
#' Supported plot types are:
#' \describe{
#'   \item{`"claims"`}{Bar plot of extracted binary claim frequencies across runs.}
#'   \item{`"textMetrics"`}{Bar plot of per-run text and timing metrics.}
#'   \item{`"claimProfile"`}{Heatmap of raw extracted claim fields across runs.}
#' }
#'
#' @param x A `wmfmRuns` object.
#' @param type Character. Plot type. One of `"claims"`, `"textMetrics"`, or
#'   `"claimProfile"`.
#' @param ... Passed through to lower-level plotting helpers.
#'
#' @return A `ggplot2` object.
#'
#' @importFrom ggplot2 ggplot aes geom_col coord_flip scale_y_continuous labs theme_minimal facet_wrap
#' @importFrom stats reorder
#' @export
plot.wmfmRuns = function(
    x,
    type = c("claims", "textMetrics", "claimProfile"),
    ...
) {
  if (!inherits(x, "wmfmRuns")) {
    stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  if (!is.list(x$runs) || length(x$runs) == 0) {
    stop("`x$runs` must be a non-empty list of run records.", call. = FALSE)
  }

  type = match.arg(type)

  if (identical(type, "claims")) {
    plotData = getWmfmRunsClaimsData(x)

    plotObj = ggplot(
      plotData,
      aes(
        x = reorder(claim, proportionPresent),
        y = proportionPresent
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = "Claim frequency across repeated runs",
        x = NULL,
        y = "Proportion of runs"
      ) +
      theme_minimal()

    return(plotObj)
  }

  if (identical(type, "claimProfile")) {
    return(plotWmfmExplanationClaimHeatmap(x = x, ...))
  }

  plotData = getWmfmRunsTextMetricsData(x)

  plotDataLong = rbind(
    data.frame(
      runId = plotData$runId,
      metric = "wordCount",
      value = plotData$wordCount
    ),
    data.frame(
      runId = plotData$runId,
      metric = "sentenceCount",
      value = plotData$sentenceCount
    ),
    data.frame(
      runId = plotData$runId,
      metric = "runElapsedSeconds",
      value = plotData$runElapsedSeconds
    )
  )

  ggplot(
    plotDataLong,
    aes(
      x = factor(runId),
      y = value
    )
  ) +
    geom_col() +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    labs(
      title = "Run metrics across repeated runs",
      x = "Run ID",
      y = "Value"
    ) +
    theme_minimal()
}
