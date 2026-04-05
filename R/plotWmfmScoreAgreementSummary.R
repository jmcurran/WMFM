#' Plot an ordinal agreement summary for WMFM score comparison
#'
#' Draws an agreement summary plot for ordinal metrics in a
#' `wmfmScoreComparison` object. Metrics can be ordered from worst to best
#' agreement using a composite disagreement score.
#'
#' The plot shows:
#' \itemize{
#'   \item weighted kappa,
#'   \item exact agreement,
#'   \item adjacent agreement, and
#'   \item mean absolute difference.
#' }
#'
#' Agreement proportions and weighted kappa are displayed on a common 0 to 1
#' scale. Mean absolute difference is shown in a separate panel.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param orderBy Character. One of `"worst"` or `"registry"`.
#'
#' @return A `ggplot` object.
#' @keywords internal
plotWmfmScoreAgreementSummary = function(
    x,
    orderBy = c("worst", "registry")
) {

  orderBy = match.arg(orderBy)

  df = x$ordinalAgreement

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("No ordinal agreement summary is available to plot.", call. = FALSE)
  }

  metricOrder = orderWmfmAgreementMetrics(df, orderBy = orderBy)

  agreementDf = data.frame(
    label = rep(df$label, times = 3),
    statistic = rep(
      c("Weighted kappa", "Adjacent agreement", "Exact agreement"),
      each = nrow(df)
    ),
    value = c(
      df$weightedKappa,
      df$proportionAdjacent,
      df$proportionEqual
    ),
    panel = "Agreement proportion / index",
    stringsAsFactors = FALSE
  )

  madDf = data.frame(
    label = df$label,
    statistic = "Mean absolute difference",
    value = df$meanAbsoluteDifference,
    panel = "Mean absolute difference",
    stringsAsFactors = FALSE
  )

  plotDf = rbind(agreementDf, madDf)
  plotDf$label = factor(plotDf$label, levels = rev(metricOrder))
  plotDf$statistic = factor(
    plotDf$statistic,
    levels = c(
      "Weighted kappa",
      "Adjacent agreement",
      "Exact agreement",
      "Mean absolute difference"
    )
  )
  plotDf$panel = factor(
    plotDf$panel,
    levels = c("Agreement proportion / index", "Mean absolute difference")
  )

  rightMethod = x$rightMethod %||% "right"
  leftMethod = x$leftMethod %||% "left"

  p = ggplot2::ggplot(
    plotDf,
    ggplot2::aes(x = value, y = label, shape = statistic)
  ) +
    ggplot2::geom_vline(
      data = data.frame(
        panel = factor("Agreement proportion / index", levels = levels(plotDf$panel)),
        xint = 0
      ),
      ggplot2::aes(xintercept = xint),
      linewidth = 0.3,
      alpha = 0.6
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(),
      cols = ggplot2::vars(panel),
      scales = "free_x",
      space = "free_x"
    ) +
    ggplot2::scale_shape_manual(
      values = c(16, 17, 15, 3),
      breaks = c(
        "Weighted kappa",
        "Adjacent agreement",
        "Exact agreement",
        "Mean absolute difference"
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::labs(
      title = "Agreement across ordinal metrics",
      subtitle = paste0(rightMethod, " versus ", leftMethod),
      x = NULL,
      y = NULL,
      shape = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      strip.background = ggplot2::element_rect(fill = "grey95"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(hjust = 1),
      plot.margin = ggplot2::margin(10, 18, 16, 10)
    )

  p
}
