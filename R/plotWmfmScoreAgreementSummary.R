#' Plot ordinal agreement summary for WMFM score comparison
#'
#' @param x A wmfmScoreComparison object
#' @param orderBy "worst" or "registry"
#'
#' @return ggplot object
#' @export
plotWmfmScoreAgreementSummary = function(x, orderBy = c("worst", "registry")) {

  orderBy = match.arg(orderBy)

  df = x$ordinalAgreement

  if (is.null(df) || nrow(df) == 0) {
    stop("No ordinal agreement summary is available to plot.", call. = FALSE)
  }

  # ---- ordering ----
  if (orderBy == "worst") {
    score =
      (1 - df$proportionAdjacent) +
      df$meanAbsoluteDifference

    ord = order(score, decreasing = TRUE)
    df = df[ord, , drop = FALSE]
  }

  df$label = factor(df$label, levels = rev(df$label))

  # ---- reshape ----
  agreementDf = data.frame(
    label = rep(df$label, 3),
    value = c(df$weightedKappa, df$proportionAdjacent, df$proportionEqual),
    statistic = factor(
      rep(c("Kappa", "Adjacent", "Exact"), each = nrow(df)),
      levels = c("Kappa", "Adjacent", "Exact")
    )
  )

  madDf = data.frame(
    label = df$label,
    value = df$meanAbsoluteDifference
  )

  library(ggplot2)

  # ---- agreement panel ----
  p1 = ggplot(agreementDf, aes(x = value, y = label, shape = statistic)) +
    geom_point(size = 3) +
    scale_x_continuous(limits = c(0, 1)) +
    geom_vline(xintercept = 0.8, linetype = "dashed", alpha = 0.3) +
    labs(
      title = "Agreement metrics",
      x = NULL,
      y = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      plot.margin = margin(5, 20, 5, 10)
    )

  # ---- MAD panel ----
  p2 = ggplot(madDf, aes(x = value, y = label)) +
    geom_point(shape = 3, size = 4) +
    geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.3) +
    labs(
      title = "Mean absolute difference",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.y = element_blank(),
      plot.margin = margin(5, 10, 5, 10)
    )

  # ---- combine ----
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for this plot.", call. = FALSE)
  }

  p = p1 + p2 + patchwork::plot_layout(widths = c(2, 1))

  p + patchwork::plot_annotation(
    title = "Agreement across ordinal metrics",
    subtitle = paste(x$rightMethod, "versus", x$leftMethod)
  )
}
