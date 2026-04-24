#' Plot a WMFM score comparison object
#'
#' Provides plotting modes for a `wmfmScoreComparison` object.
#'
#' \describe{
#'   \item{`type = "overall"`}{A Bland-Altman plot for paired overall scores.}
#'   \item{`type = "agreement"`}{An ordinal-agreement summary plot showing
#'   exact agreement, adjacent agreement, weighted kappa, and mean absolute
#'   difference.}
#'   \item{`type = "heatmap"`}{A run-by-metric disagreement heatmap based on
#'   run-level comparison pairs.}
#' }
#'
#' @param x A `wmfmScoreComparison` object.
#' @param type Character. One of `"agreement"`, `"overall"`, or `"heatmap"`.
#' @param ... Additional arguments passed to the underlying helper.
#'
#' @return A `ggplot` object.
#' @export
plot.wmfmScoreComparison = function(
    x,
    type = c("agreement", "overall", "heatmap"),
    ...
) {

  if (!inherits(x, "wmfmScoreComparison")) {
    stop("`x` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  type = match.arg(type)

  if (type == "overall") {
    df = x$pairedOverallScores
    summary = x$overallSummary

    if (!is.data.frame(df) || nrow(df) == 0) {
      stop("No paired overall scores are available to plot.", call. = FALSE)
    }

    if (is.null(summary)) {
      stop("No overall summary is available to plot.", call. = FALSE)
    }

    leftMethod = x$leftMethod %||% "left"
    rightMethod = x$rightMethod %||% "right"

    return(
      ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$meanOverallScore, y = .data$differenceOverallScore)
      ) +
        ggplot2::geom_point(size = 2.6, alpha = 0.85) +
        ggplot2::geom_hline(
          yintercept = summary$meanDifferenceRightMinusLeft,
          linewidth = 0.5
        ) +
        ggplot2::geom_hline(
          yintercept = summary$loaLower,
          linetype = 2,
          linewidth = 0.4
        ) +
        ggplot2::geom_hline(
          yintercept = summary$loaUpper,
          linetype = 2,
          linewidth = 0.4
        ) +
        ggplot2::labs(
          title = "Overall score agreement",
          subtitle = paste0(
            "Bland-Altman view: ",
            rightMethod,
            " minus ",
            leftMethod
          ),
          x = "Mean overall score",
          y = paste0(
            "Difference in overall score (",
            rightMethod,
            " - ",
            leftMethod,
            ")"
          )
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank()
        )
    )
  }

  if (type == "agreement") {
    return(plotWmfmScoreAgreementSummary(x = x, ...))
  }

  plotWmfmScoreHeatmap(x = x, ...)
}
