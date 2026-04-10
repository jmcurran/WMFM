#' Plot a WMFM score stability object
#'
#' Creates a visual summary of within-method score stability.
#'
#' @param x A `wmfmScoreStability` object.
#' @param type Character. One of `"continuous"`, `"ordinal"`, or `"binary"`.
#' @param metric Character or `NULL`. Optional metric filter.
#' @param value Character. Quantity to plot. Allowed values depend on `type`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_col coord_flip labs theme_bw
#' @importFrom ggplot2 facet_wrap position_dodge
plot.wmfmScoreStability = function(
    x,
    type = c("continuous", "ordinal", "binary"),
    metric = NULL,
    value = NULL,
    ...
) {

  type = match.arg(type)

  if (!inherits(x, "wmfmScoreStability")) {
    stop("`x` must inherit from `wmfmScoreStability`.", call. = FALSE)
  }

  getPlotDf = function(type) {
    if (identical(type, "continuous")) {
      return(x$continuousStability)
    }

    if (identical(type, "ordinal")) {
      return(x$ordinalStability)
    }

    x$binaryStability
  }

  plotDf = getPlotDf(type)

  if (is.null(plotDf) || !is.data.frame(plotDf) || nrow(plotDf) == 0) {
    stop("No stability data are available for this plot type.", call. = FALSE)
  }

  if (!is.null(metric)) {
    if (!is.character(metric)) {
      stop("`metric` must be `NULL` or a character vector.", call. = FALSE)
    }

    plotDf = plotDf[plotDf$metric %in% metric, , drop = FALSE]

    if (nrow(plotDf) == 0) {
      stop("No matching metrics were found after filtering.", call. = FALSE)
    }
  }

  if (is.null(value)) {
    value =
      if (identical(type, "continuous")) {
        "sd"
      } else if (identical(type, "ordinal")) {
        "modalProportion"
      } else {
        "trueRate"
      }
  }

  allowedValues =
    if (identical(type, "continuous")) {
      c("sd", "range", "mean")
    } else if (identical(type, "ordinal")) {
      c("modalProportion", "range", "mean", "sd")
    } else {
      c("trueRate", "modalProportion")
    }

  if (!value %in% allowedValues) {
    stop(
      "`value` must be one of: ",
      paste(allowedValues, collapse = ", "),
      call. = FALSE
    )
  }

  if (!value %in% names(plotDf)) {
    stop(
      "Selected `value` column is not present in the stability data.",
      call. = FALSE
    )
  }

  plotDf$plotValue = suppressWarnings(as.numeric(plotDf[[value]]))

  if (all(is.na(plotDf$plotValue))) {
    stop("Selected plot values are all missing.", call. = FALSE)
  }

  plotDf$metric = factor(
    plotDf$metric,
    levels = rev(unique(plotDf$metric[order(plotDf$plotValue)]))
  )

  if (identical(type, "continuous")) {
    return(
      ggplot2::ggplot(
        plotDf,
        ggplot2::aes(x = metric, y = .data$plotValue, fill = .data$method)
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = NULL,
          y = value,
          fill = "Method",
          title = paste("Continuous score stability:", value)
        ) +
        ggplot2::theme_bw()
    )
  }

  if (identical(type, "ordinal")) {
    return(
      ggplot2::ggplot(
        plotDf,
        ggplot2::aes(x = .data$metric, y = .data$plotValue, fill = .data$method)
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = NULL,
          y = value,
          fill = "Method",
          title = paste("Ordinal score stability:", value)
        ) +
        ggplot2::theme_bw()
    )
  }

  ggplot2::ggplot(
    plotDf,
    ggplot2::aes(x = .data$metric, y = .data$plotValue, fill = .data$method)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = .data$value,
      fill = "Method",
      title = paste("Binary score stability:", value)
    ) +
    ggplot2::theme_bw()
}
