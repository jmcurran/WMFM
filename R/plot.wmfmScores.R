#' Plot a WMFM scores object
#'
#' Provides plotting methods for a `wmfmScores` object returned by `score()`.
#' Score heatmaps use a continuous colour scale and, by default, include only
#' the 0–2 dimension scores to avoid distortion from the 0–100 overall score.
#'
#' @param x A `wmfmScores` object.
#' @param method Character. Scoring method to plot. One of "deterministic" or
#'   "llm".
#' @param type Character. One of "scores" or "overall".
#' @param fieldColumns Optional character vector of score columns to plot when
#'   type = "scores".
#' @param ... Additional arguments for future extensibility.
#'
#' @return A ggplot2 object.
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
    stop("No scores are available for method `", method, "`.", call. = FALSE)
  }

  if (!is.list(scoreList) || length(scoreList) == 0) {
    stop("Stored scores for method `", method, "` are empty.", call. = FALSE)
  }

  plotDf = do.call(
    rbind,
    lapply(scoreList, function(oneScore) {
      as.data.frame(oneScore, stringsAsFactors = FALSE)
    })
  )
  rownames(plotDf) = NULL
  plotDf$runId = seq_len(nrow(plotDf))

  # ---- IMPORTANT CHANGE: exclude overallScore by default ----
  defaultScoreFields = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore"
  )

  if (is.null(fieldColumns)) {
    fieldColumns = defaultScoreFields[defaultScoreFields %in% names(plotDf)]
  }

  if (length(fieldColumns) == 0) {
    stop("No score fields are available to plot.", call. = FALSE)
  }

  if (identical(type, "scores")) {

    longDf = do.call(
      rbind,
      lapply(fieldColumns, function(metricName) {
        data.frame(
          runId = plotDf$runId,
          metric = metricName,
          value = suppressWarnings(as.numeric(plotDf[[metricName]])),
          stringsAsFactors = FALSE
        )
      })
    )

    longDf = longDf[!is.na(longDf$value), , drop = FALSE]

    prettyMetricMap = c(
      factualScore = "Factual score",
      inferenceScore = "Inference score",
      completenessScore = "Completeness score",
      clarityScore = "Clarity score",
      calibrationScore = "Calibration score",
      overallScore = "Overall score"
    )

    metricLabels = fieldColumns
    matched = intersect(names(prettyMetricMap), metricLabels)
    metricLabels[match(matched, fieldColumns)] = prettyMetricMap[matched]

    longDf$metric = factor(longDf$metric, levels = fieldColumns, labels = metricLabels)

    return(
      ggplot2::ggplot(
        longDf,
        ggplot2::aes(x = metric, y = runId, fill = value)
      ) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.35) +
        ggplot2::scale_fill_gradientn(
          colours = c("#F7FBFF", "#6BAED6", "#08306B")
        ) +
        ggplot2::labs(
          title = paste("WMFM dimension score heatmap:", method),
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

  # ---- overall plot ----
  if (!"overallScore" %in% names(plotDf)) {
    stop("`overallScore` is not available.", call. = FALSE)
  }

  plotDf$overallScore = suppressWarnings(as.numeric(plotDf$overallScore))

  ggplot2::ggplot(
    plotDf,
    ggplot2::aes(x = runId, y = overallScore)
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
