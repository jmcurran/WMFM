#' Plot an ordinal agreement summary for WMFM score comparison
#'
#' Draws an agreement summary plot for ordinal metrics in a
#' `wmfmScoreComparison` object. Metrics are ordered from worst to best
#' agreement using a simple composite disagreement score.
#'
#' The plot shows:
#' \itemize{
#'   \item weighted kappa,
#'   \item exact agreement,
#'   \item adjacent agreement, and
#'   \item mean absolute difference.
#' }
#'
#' Exact agreement, adjacent agreement, and weighted kappa are displayed on a
#' common 0-1 scale. Mean absolute difference is shown in a separate facet so
#' that it keeps its original interpretation.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param orderBy Character. Currently only `"worst"` is supported.
#'
#' @return A `ggplot` object.
#' @keywords internal
plotWmfmScoreAgreementSummary = function(x, orderBy = c("worst")) {
  orderBy = match.arg(orderBy)

  df = x$ordinalAgreement

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("No ordinal agreement summary is available to plot.", call. = FALSE)
  }

  disagreementScore =
    (1 - df$weightedKappa) +
    (1 - df$proportionAdjacent) +
    (1 - df$proportionEqual) +
    df$meanAbsoluteDifference

  if (identical(orderBy, "worst")) {
    metricOrder = df$label[order(disagreementScore, decreasing = TRUE)]
  } else {
    metricOrder = df$label
  }

  scale01Df = data.frame(
    label = rep(df$label, times = 3),
    statistic = rep(
      c("Weighted kappa", "Adjacent agreement", "Exact agreement"),
      each = nrow(df)
    ),
    value = c(df$weightedKappa, df$proportionAdjacent, df$proportionEqual),
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

  plotDf = rbind(scale01Df, madDf)
  plotDf$label = factor(plotDf$label, levels = rev(unique(metricOrder)))
  plotDf$statistic = factor(
    plotDf$statistic,
    levels = c(
      "Weighted kappa",
      "Adjacent agreement",
      "Exact agreement",
      "Mean absolute difference"
    )
  )

  ggplot2::ggplot(
    plotDf,
    ggplot2::aes(x = value, y = label, shape = statistic)
  ) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::facet_wrap(~ panel, scales = "free_x") +
    ggplot2::labs(
      title = "Agreement across ordinal metrics",
      subtitle = paste0(
        x$rightMethod %||% "right",
        " versus ",
        x$leftMethod %||% "left"
      ),
      x = NULL,
      y = NULL,
      shape = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.background = ggplot2::element_rect(fill = "grey95"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
