#' Plot a metric comparison summary
#'
#' Plots the relationship between disagreement and deterministic ease for a
#' `metricComparisonSummary` object.
#'
#' @param x A `metricComparisonSummary` object created by
#'   `summariseMetricComparison()`.
#' @param type Plot type. One of `"scatter"`, `"lollipop"`, or `"diagnostic"`.
#' @param metricType Optional metric type filter. One of `NULL`, `"ordinal"`,
#'   `"binary"`, or `"continuous"`.
#' @param labelPoints Logical. Should points be labelled in the scatter-style
#'   plots?
#' @param orderBy Ordering for the lollipop plot. One of `"disagreement"` or
#'   `"ease"`.
#' @param disagreementThreshold Numeric threshold used by `type = "diagnostic"`
#'   to decide which metrics to label and highlight.
#' @param easeThreshold Numeric threshold used by `type = "diagnostic"` to
#'   decide which metrics to label and highlight.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_segment labs theme_minimal
#'   theme element_text coord_cartesian scale_x_continuous scale_colour_manual
#'   geom_vline geom_hline
plot.metricComparisonSummary = function(
  x,
  type = c("scatter", "lollipop", "diagnostic"),
  metricType = NULL,
  labelPoints = TRUE,
  orderBy = c("disagreement", "ease"),
  disagreementThreshold = 0.5,
  easeThreshold = 0.8,
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

  if (type %in% c("scatter", "diagnostic")) {

    if (identical(type, "diagnostic")) {
      df$isFlagged =
        (!is.na(df$meanAbsoluteDifference) & df$meanAbsoluteDifference >= disagreementThreshold) |
        (!is.na(df$modalProportionDeterministic) & df$modalProportionDeterministic >= easeThreshold)

      p = ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data$modalProportionDeterministic,
          y = .data$meanAbsoluteDifference,
          colour = .data$isFlagged
        )
      ) +
        ggplot2::geom_vline(xintercept = easeThreshold, linetype = 2, alpha = 0.5) +
        ggplot2::geom_hline(yintercept = disagreementThreshold, linetype = 2, alpha = 0.5) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::scale_colour_manual(
          values = c("FALSE" = "grey40", "TRUE" = "black"),
          guide = "none"
        ) +
        ggplot2::labs(
          title = "Diagnostic view: disagreement versus deterministic ease",
          subtitle = paste0(
            "Flagged if mean absolute difference >= ",
            disagreementThreshold,
            " or modal proportion >= ",
            easeThreshold
          ),
          x = "Deterministic modal proportion",
          y = "Mean absolute difference"
        )
    } else {
      p = ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data$modalProportionDeterministic,
          y = .data$meanAbsoluteDifference
        )
      ) +
        ggplot2::geom_point() +
        ggplot2::labs(
          title = "Disagreement versus deterministic ease",
          subtitle = "Higher modal proportion suggests easier deterministic scoring",
          x = "Deterministic modal proportion",
          y = "Mean absolute difference"
        )
    }

    p = p +
      ggplot2::scale_x_continuous(expand = c(0.02, 0.06)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )

    if (isTRUE(labelPoints)) {
      if (identical(type, "diagnostic")) {
        labelDf = df[df$isFlagged, , drop = FALSE]
      } else {
        labelDf = df
      }

      if (nrow(labelDf) > 0) {
        p = p + ggrepel::geom_text_repel(
          data = labelDf,
          ggplot2::aes(
            x = .data$modalProportionDeterministic,
            y = .data$meanAbsoluteDifference,
            label = .data$label
          ),
          inherit.aes = FALSE,
          size = 3,
          max.overlaps = Inf,
          box.padding = 0.3,
          point.padding = 0.2,
          segment.alpha = 0.5
        ) +
          ggplot2::coord_cartesian(clip = "off")
      }
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

  ggplot2::ggplot(df, ggplot2::aes(y = .data$label, x = .data$meanAbsoluteDifference)) +
    ggplot2::geom_segment(
      ggplot2::aes(yend = .data$label, x = 0, xend = .data$meanAbsoluteDifference)
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
