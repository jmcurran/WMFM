#' Plot a WMFM scores object
#'
#' Provides plotting methods for a `wmfmScores` object returned by `score()`.
#' Score heatmaps use a continuous colour scale and, by default, include only
#' the 0 to 2 dimension scores to avoid distortion from the 0 to 100 overall
#' score.
#'
#' @param x A `wmfmScores` object.
#' @param method Optional character. Scoring method to plot. One of
#'   "deterministic" or "llm". If omitted, an available method is chosen
#'   automatically.
#' @param type Character. One of "scores", "overall", or "summary".
#' @param fieldColumns Optional character vector of score columns to plot when
#'   `type = "scores"` or `type = "summary"`.
#' @param ... Additional arguments for future extensibility.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_line geom_point geom_segment
#' @importFrom ggplot2 scale_fill_gradient2 scale_x_continuous labs
#' @importFrom ggplot2 theme_minimal theme_bw theme element_text element_blank
#' @importFrom ggplot2 coord_cartesian
plot.wmfmScores = function(
    x,
    method = NULL,
    type = c("scores", "overall", "summary"),
    fieldColumns = NULL,
    ...
) {

  if (!inherits(x, "wmfmScores")) {
    stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
  }

  type = match.arg(type)

  if (is.null(x$scores) || !is.list(x$scores)) {
    stop("`x` does not contain a valid `scores` element.", call. = FALSE)
  }

  availableMethods = names(x$scores)
  availableMethods = availableMethods[!is.na(availableMethods) & nzchar(availableMethods)]

  if (is.null(method)) {
    if ("deterministic" %in% availableMethods) {
      method = "deterministic"
    } else if ("llm" %in% availableMethods) {
      method = "llm"
    } else {
      stop("No scoring methods are available to plot.", call. = FALSE)
    }
  } else {
    method = match.arg(method, choices = c("deterministic", "llm"))
  }

  scoreList = x$scores[[method]]

  if (is.null(scoreList)) {
    stop("No scores are available for method `", method, "`.", call. = FALSE)
  }

  if (!is.list(scoreList) || length(scoreList) == 0) {
    stop("Stored scores for method `", method, "` are empty.", call. = FALSE)
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
    "calibrationScore"
  )

  if (is.null(fieldColumns)) {
    fieldColumns = defaultScoreFields[defaultScoreFields %in% names(plotDf)]
  }

  if (length(fieldColumns) == 0) {
    stop("No score fields are available to plot.", call. = FALSE)
  }

  missingColumns = setdiff(fieldColumns, names(plotDf))

  if (length(missingColumns) > 0) {
    stop(
      "The following `fieldColumns` are not present in the selected scores: ",
      paste(missingColumns, collapse = ", "),
      call. = FALSE
    )
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
  matched = intersect(names(prettyMetricMap), metricLabels)
  metricLabels[match(matched, fieldColumns)] = prettyMetricMap[matched]

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

    rownames(longDf) = NULL
    longDf = longDf[!is.na(longDf$value), , drop = FALSE]

    if (nrow(longDf) == 0) {
      stop("No non-missing score values are available to plot.", call. = FALSE)
    }

    longDf$metric = factor(longDf$metric, levels = fieldColumns, labels = metricLabels)

    return(
      ggplot2::ggplot(
        longDf,
        ggplot2::aes(x = metric, y = runId, fill = value)
      ) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.35) +
        scale_fill_gradient2(
          low = "#F7FBFF",
          mid = "#6BAED6",
          high = "#08306B",
          midpoint = 1,
          limits = c(0, 2),
          oob = scales::squish
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

  if (identical(type, "summary")) {
    summaryPieces = lapply(seq_along(fieldColumns), function(i) {
      metricName = fieldColumns[i]
      values = suppressWarnings(as.numeric(plotDf[[metricName]]))
      values = values[!is.na(values)]

      if (length(values) == 0) {
        return(NULL)
      }

      data.frame(
        metric = metricLabels[i],
        meanScore = mean(values),
        minScore = min(values),
        maxScore = max(values),
        sdScore = if (length(values) > 1) {
          stats::sd(values)
        } else {
          0
        },
        stringsAsFactors = FALSE
      )
    })

    summaryDf = do.call(rbind, summaryPieces)

    if (is.null(summaryDf) || nrow(summaryDf) == 0) {
      stop("No non-missing score values are available to summarise.", call. = FALSE)
    }

    summaryDf$metric = factor(
      summaryDf$metric,
      levels = rev(metricLabels[metricLabels %in% summaryDf$metric])
    )

    maxValue = max(summaryDf$maxScore, na.rm = TRUE)

    return(
      ggplot2::ggplot(
        summaryDf,
        ggplot2::aes(y = metric, x = meanScore)
      ) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = minScore,
            xend = maxScore,
            yend = metric
          ),
          linewidth = 0.6
        ) +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::coord_cartesian(
          xlim = c(0, maxValue * 1.05)
        ) +
        ggplot2::labs(
          title = paste("WMFM score summary:", method),
          subtitle = "Points show means; horizontal lines show run-to-run range",
          x = "Score",
          y = NULL
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )
    )
  }

  if (!"overallScore" %in% names(plotDf)) {
    stop("`overallScore` is not available.", call. = FALSE)
  }

  plotDf$overallScore = suppressWarnings(as.numeric(plotDf$overallScore))

  if (all(is.na(plotDf$overallScore))) {
    stop("`overallScore` is all missing.", call. = FALSE)
  }

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
