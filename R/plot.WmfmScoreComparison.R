
#' Plot a score comparison object
#'
#' Creates a visual summary of agreement between deterministic and LLM scoring.
#'
#' @param x Object produced by `compareScores()`.
#' @param type Character. One of `"agreement"` or `"overall"`.
#' @param labelSize Numeric text size used for metric labels in the agreement
#'   plot. Defaults to `10`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_vline geom_hline
#' @importFrom ggplot2 coord_cartesian labs theme_bw theme element_text
#' @importFrom ggplot2 position_dodge
plot.WmfmScoreComparison = function(
    x,
    type = c("agreement", "overall"),
    labelSize = 10,
    ...
) {

  type = match.arg(type)

  if (!is.numeric(labelSize) || length(labelSize) != 1 || is.na(labelSize) || labelSize <= 0) {
    stop("`labelSize` must be a single positive number.", call. = FALSE)
  }

  if (identical(type, "agreement")) {
    metricAgreement = x$metricAgreement

    if (is.null(metricAgreement) || nrow(metricAgreement) == 0) {
      stop("`x$metricAgreement` is empty.", call. = FALSE)
    }

    plotDf = rbind(
      data.frame(
        metric = metricAgreement$metric,
        measure = "Proportion equal",
        value = metricAgreement$proportionEqual,
        label = formatC(metricAgreement$proportionEqual, digits = 2, format = "f"),
        stringsAsFactors = FALSE
      ),
      data.frame(
        metric = metricAgreement$metric,
        measure = "Mean difference",
        value = metricAgreement$meanDifference,
        label = ifelse(
          is.na(metricAgreement$meanDifference),
          "NA",
          formatC(metricAgreement$meanDifference, digits = 2, format = "f")
        ),
        stringsAsFactors = FALSE
      )
    )

    plotDf$metric = factor(
      plotDf$metric,
      levels = rev(metricAgreement$metric[order(metricAgreement$proportionEqual)])
    )

    return(
      ggplot2::ggplot(plotDf, ggplot2::aes(x = value, y = metric, shape = measure)) +
        ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_text(
          ggplot2::aes(label = label),
          hjust = -0.1,
          size = labelSize / 3,
          position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, linetype = 2) +
        ggplot2::coord_cartesian(
          xlim = c(
            min(-0.05, min(plotDf$value, na.rm = TRUE) - 0.05),
            max(1.05, max(plotDf$value, na.rm = TRUE) + 0.10)
          )
        ) +
        ggplot2::labs(
          x = "Comparison value",
          y = NULL,
          shape = NULL,
          title = "Deterministic versus LLM scoring comparison"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = labelSize),
          legend.position = "bottom"
        )
    )
  }

  overallDf = x$pairedOverallScores

  if (is.null(overallDf) || nrow(overallDf) == 0) {
    stop(
      "No paired overall scores are available for plotting.",
      call. = FALSE
    )
  }

  meanDiff = mean(overallDf$differenceOverallScore, na.rm = TRUE)
  sdDiff = stats::sd(overallDf$differenceOverallScore, na.rm = TRUE)
  lowerLoA = meanDiff - 1.96 * sdDiff
  upperLoA = meanDiff + 1.96 * sdDiff

  ggplot2::ggplot(
    overallDf,
    ggplot2::aes(x = meanOverallScore, y = differenceOverallScore)
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = meanDiff, linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = lowerLoA, linewidth = 0.4, linetype = 2) +
    ggplot2::geom_hline(yintercept = upperLoA, linewidth = 0.4, linetype = 2) +
    ggplot2::labs(
      x = "Mean of deterministic and LLM overall scores",
      y = "LLM - deterministic overall score",
      title = "Overall score comparison"
    ) +
    ggplot2::theme_bw()
}
