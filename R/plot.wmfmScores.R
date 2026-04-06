#' Plot a WMFM scores object
#'
#' Provides plotting methods for a `wmfmScores` object returned by `score()`.
#' Score heatmaps are drawn using a continuous colour scale, which is more
#' appropriate for numeric score fields than the categorical heatmap used for
#' claim and judged fields.
#'
#' @param x A `wmfmScores` object.
#' @param method Character. Scoring method to plot. One of `"deterministic"` or
#'   `"llm"`.
#' @param type Character. One of `"scores"` or `"overall"`.
#' @param fieldColumns Optional character vector of score columns to plot when
#'   `type = "scores"`.
#' @param ... Additional arguments passed to the underlying plotting helper for
#'   future extensibility.
#'
#' @return For `type = "scores"` or `type = "overall"`, a `ggplot2` object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_line geom_point
#' @importFrom ggplot2 scale_fill_gradientn scale_x_continuous labs
#' @importFrom ggplot2 theme_minimal theme_bw theme element_text element_blank
plot.wmfmScores = function(
    x,
    method = c("deterministic", "llm"),
    type = c("scores", "overall"),
    fieldColumns = NULL,
    ...
) {

  if (!inherits(x, "wmfmScores")) {
    stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
  }

  method = match.arg(method)
  type = match.arg(type)

  if (is.null(x$scores) || !is.list(x$scores)) {
    stop("`x` does not contain a valid `scores` element.", call. = FALSE)
  }

  scoreList = x$scores[[method]]

  if (is.null(scoreList)) {
    stop(
      "No scores are available for method `", method, "`.",
      call. = FALSE
    )
  }

  if (!is.list(scoreList) || length(scoreList) == 0) {
    stop(
      "Stored scores for method `", method, "` are empty.",
      call. = FALSE
    )
  }

  badIndex = which(
    !vapply(
      scoreList,
      function(oneScore) {
        is.list(oneScore) && !is.null(names(oneScore))
      },
      logical(1)
    )
  )

  if (length(badIndex) > 0) {
    stop(
      "Stored scores for method `", method, "` must be a list of named lists.",
      call. = FALSE
    )
  }

  plotDf = do.call(
    rbind,
    lapply(scoreList, function(oneScore) {
      as.data.frame(oneScore, stringsAsFactors = FALSE)
    })
  )
  rownames(plotDf) = NULL
  plotDf$runId = seq_len(nrow(plotDf))

  defaultScoreFields = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore",
    "overallScore"
  )

  if (is.null(fieldColumns)) {
    fieldColumns = defaultScoreFields[defaultScoreFields %in% names(plotDf)]
  }

  if (length(fieldColumns) == 0) {
    stop(
      "No score fields are available to plot for method `", method, "`.",
      call. = FALSE
    )
  }

  missingColumns = setdiff(fieldColumns, names(plotDf))

  if (length(missingColumns) > 0) {
    stop(
      "The following `fieldColumns` are not present in the selected scores: ",
      paste(missingColumns, collapse = ", "),
      call. = FALSE
    )
  }

  if (identical(type, "scores")) {
    longPieces = lapply(fieldColumns, function(metricName) {
      data.frame(
        runId = plotDf$runId,
        metric = metricName,
        value = suppressWarnings(as.numeric(plotDf[[metricName]])),
        stringsAsFactors = FALSE
      )
    })

    longDf = do.call(rbind, longPieces)
    rownames(longDf) = NULL
    longDf = longDf[!is.na(longDf$value), , drop = FALSE]

    if (nrow(longDf) == 0) {
      stop("No non-missing score values are available to plot.", call. = FALSE)
    }

    prettyMetricMap = c(
      factualScore = "Factual score",
      inferenceScore = "Inference score",
      completenessScore = "Completeness score",
      clarityScore = "Clarity score",
      calibrationScore = "Calibration score",
      overallScore = "Overall score"
    )

    metricLabels = fieldColumns
    matchedMetrics = intersect(names(prettyMetricMap), metricLabels)
    metricLabels[match(matchedMetrics, fieldColumns)] = prettyMetricMap[matchedMetrics]

    longDf$metric = factor(longDf$metric, levels = fieldColumns, labels = metricLabels)

    return(
      ggplot2::ggplot(
        longDf,
        ggplot2::aes(
          x = metric,
          y = runId,
          fill = value
        )
      ) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.35) +
        ggplot2::scale_fill_gradientn(
          colours = c("#F7FBFF", "#6BAED6", "#08306B")
        ) +
        ggplot2::labs(
          title = paste("WMFM score heatmap:", method),
          x = NULL,
          y = "Run",
          fill = "Score"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
          plot.title = ggplot2::element_text(face = "bold")
        )
    )
  }

  if (!"overallScore" %in% names(plotDf)) {
    stop(
      "`overallScore` is not available for method `", method, "`.",
      call. = FALSE
    )
  }

  plotDf$overallScore = suppressWarnings(as.numeric(plotDf$overallScore))

  if (all(is.na(plotDf$overallScore))) {
    stop(
      "`overallScore` is all missing for method `", method, "`.",
      call. = FALSE
    )
  }

  ggplot2::ggplot(
    plotDf,
    ggplot2::aes(
      x = runId,
      y = overallScore
    )
  ) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_x_continuous(breaks = plotDf$runId) +
    ggplot2::labs(
      title = paste("Overall score by run:", method),
      x = "Run",
      y = "Overall score"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
}
