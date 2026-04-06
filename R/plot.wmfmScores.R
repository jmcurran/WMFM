#' Plot a WMFM scores object
#'
#' Provides plotting methods for a `wmfmScores` object returned by `score()`.
#' The default score heatmap reuses the existing repeated-run score heatmap
#' machinery after reconstructing a small plotting data frame from the stored
#' score records.
#'
#' @param x A `wmfmScores` object.
#' @param method Character. Scoring method to plot. One of `"deterministic"` or
#'   `"llm"`.
#' @param type Character. One of `"scores"` or `"overall"`.
#' @param fieldColumns Optional character vector of score columns to plot when
#'   `type = "scores"`.
#' @param sortRows Logical. Passed to `plotWmfmExplanationClaimHeatmap()` when
#'   `type = "scores"`.
#' @param sortColumns Character. Passed to `plotWmfmExplanationClaimHeatmap()`
#'   when `type = "scores"`.
#' @param ... Additional arguments passed to the underlying plotting helper.
#'
#' @return For `type = "scores"`, invisibly returns the object produced by
#'   `plotWmfmExplanationClaimHeatmap()`. For `type = "overall"`, returns a
#'   `ggplot2` object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_continuous labs
#' @importFrom ggplot2 theme_bw theme element_blank
plot.wmfmScores = function(
    x,
    method = c("deterministic", "llm"),
    type = c("scores", "overall"),
    fieldColumns = NULL,
    sortRows = FALSE,
    sortColumns = c("schema", "purity", "none"),
    ...
) {

  if (!inherits(x, "wmfmScores")) {
    stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
  }

  method = match.arg(method)
  type = match.arg(type)
  sortColumns = match.arg(sortColumns)

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
    return(
      plotWmfmExplanationClaimHeatmap(
        runsDf = plotDf,
        fieldColumns = fieldColumns,
        plotType = "scores",
        runIdColumn = "runId",
        sortRows = sortRows,
        sortColumns = sortColumns,
        main = paste("WMFM score heatmap:", method),
        ...
      )
    )
  }

  if (!"overallScore" %in% names(plotDf)) {
    stop(
      "`overallScore` is not available for method `", method, "`.",
      call. = FALSE
    )
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
