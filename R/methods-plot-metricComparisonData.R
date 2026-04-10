#' Plot a metric comparison data object
#'
#' Provides quick diagnostic plots for the run-level output returned by
#' `getMetricComparisonData()`.
#'
#' @param x A `metricComparisonData` object.
#' @param type Plot type. One of `"confusion"` or `"runs"`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient labs
#'   theme_minimal theme element_text geom_segment geom_point scale_x_continuous
#'   scale_y_continuous
#' @importFrom rlang .data
plot.metricComparisonData = function(
  x,
  type = c("confusion", "runs"),
  ...
) {

  if (!inherits(x, "metricComparisonData")) {
    stop("`x` must inherit from `metricComparisonData`.", call. = FALSE)
  }

  type = match.arg(type)

  df = as.data.frame(x, stringsAsFactors = FALSE)

  otherCols = setdiff(
    names(df),
    c(
      "runId",
      "detValue",
      "llmMinusDet",
      "detMinusLlm",
      "difference",
      "absDifference",
      "agreementClass"
    )
  )

  valueCols = otherCols[grepl("Value$", otherCols)]

  if (length(valueCols) != 1) {
    stop("Could not determine the non-deterministic value column.", call. = FALSE)
  }

  otherValueCol = valueCols[1]
  otherLabel = sub("Value$", "", otherValueCol)
  metricName = attr(x, "metric")

  if (identical(type, "confusion")) {
    plotDf = as.data.frame(table(
      detValue = df$detValue,
      otherValue = df[[otherValueCol]]
    ), stringsAsFactors = FALSE)

    plotDf$Freq = as.numeric(plotDf$Freq)

    return(
      ggplot2::ggplot(
        plotDf,
        ggplot2::aes(x = .data$detValue, y = .data$otherValue, fill = .data$Freq)
      ) +
        ggplot2::geom_tile(colour = "white") +
        ggplot2::geom_text(ggplot2::aes(label = .data$Freq)) +
        ggplot2::scale_fill_gradient(low = "grey90", high = "grey20") +
        ggplot2::labs(
          title = paste("Cross-tab for", metricName),
          subtitle = paste(otherLabel, "versus deterministic"),
          x = "Deterministic value",
          y = paste0(otherLabel, " value"),
          fill = "Count"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold")
        )
    )
  }

  df$runId = as.numeric(df$runId)
  df$otherValue = df[[otherValueCol]]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$runId)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = .data$runId,
        y = .data$detValue,
        yend = .data$otherValue
      ),
      colour = "grey60"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$detValue),
      size = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$otherValue),
      shape = 17,
      size = 2
    ) +
    ggplot2::scale_x_continuous(breaks = df$runId) +
    ggplot2::labs(
      title = paste("Run-level comparison for", metricName),
      subtitle = paste("Points joined within run:", otherLabel, "versus deterministic"),
      x = "Run",
      y = "Metric value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )
}
