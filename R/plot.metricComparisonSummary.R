#' Plot a metric comparison summary
#'
#' Plots the relationship between disagreement and deterministic ease for a
#' `metricComparisonSummary` object.
#'
#' @param x A `metricComparisonSummary` object created by
#'   `summariseMetricComparison()`.
#' @param type Plot type. One of `"scatter"` or `"lollipop"`.
#' @param metricType Optional metric type filter. One of `NULL`, `"ordinal"`,
#'   `"binary"`, or `"continuous"`.
#' @param labelPoints Logical. Should points be labelled in the scatter plot?
#' @param orderBy Ordering for the lollipop plot. One of `"disagreement"` or
#'   `"ease"`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text labs
#'   theme_minimal theme element_text coord_cartesian
plot.metricComparisonSummary = function(
  x,
  type = c("scatter", "lollipop"),
  metricType = NULL,
  labelPoints = TRUE,
  orderBy = c("disagreement", "ease"),
  ...
) {

  if (!inherits(x, "metricComparisonSummary")) {
    stop("`x` must inherit from `metricComparisonSummary`.", call. = FALSE)
  }

  type = match.arg(type)
  orderBy = match.arg(orderBy)

  df = x

  if (!is.null(metricType)) {
    if (!metricType %in% c("ordinal", "binary", "continuous")) {
      stop("`metricType` must be NULL, 'ordinal', 'binary', or 'continuous'.", call. = FALSE)
    }

    df = df[df$metricType == metricType, , drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No rows available to plot after filtering.", call. = FALSE)
  }

  if (!"meanAbsoluteDifference" %in% names(df)) {
    stop("`x` does not contain `meanAbsoluteDifference`.", call. = FALSE)
  }

  if (all(is.na(df$meanAbsoluteDifference))) {
    stop("`meanAbsoluteDifference` is all missing for the selected rows.", call. = FALSE)
  }

  if (type == "scatter") {
    p = ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = modalProportionDeterministic,
        y = meanAbsoluteDifference
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::labs(
        title = "Disagreement versus deterministic ease",
        subtitle = "Higher modal proportion suggests easier deterministic scoring",
        x = "Deterministic modal proportion",
        y = "Mean absolute difference"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )

    if (isTRUE(labelPoints)) {
      p = p + ggplot2::geom_text(
        ggplot2::aes(label = label),
        hjust = -0.05,
        size = 3
      ) +
        ggplot2::coord_cartesian(clip = "off")
    }

    return(p)
  }

  if (identical(orderBy, "disagreement")) {
    df = df[order(-df$meanAbsoluteDifference, df$label), , drop = FALSE]
  } else {
    easeScore = -df$modalProportionDeterministic + df$entropyDeterministic
    df = df[order(easeScore, df$label), , drop = FALSE]
  }

  df$label = factor(df$label, levels = rev(df$label))

  ggplot2::ggplot(df, ggplot2::aes(y = label, x = meanAbsoluteDifference)) +
    ggplot2::geom_segment(
      ggplot2::aes(yend = label, x = 0, xend = meanAbsoluteDifference)
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Metric disagreement",
      subtitle = if (identical(orderBy, "disagreement")) {
        "Ordered by disagreement"
      } else {
        "Ordered by deterministic ease"
      },
      x = "Mean absolute difference",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(size = 10)
    )
}
