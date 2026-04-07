#' Plot a raw-claim heatmap for WMFM runs
#'
#' Draws a run-by-field heatmap for a `wmfmRuns` object using raw extracted
#' claim fields only. Rows represent runs and columns represent claim fields.
#' This function is intended for visualising run-to-run variation in
#' interpretation patterns and no longer supports judged fields or aggregate
#' scores.
#'
#' @param x A `wmfmRuns` object.
#' @param fieldColumns Optional character vector of raw claim fields to plot.
#'   If `NULL`, a default raw-only claim profile is used.
#' @param naLabel Character label used for missing values.
#' @param main Character plot title.
#' @param xlab Character x-axis label.
#' @param ylab Character y-axis label.
#' @param prettyFieldLabels Logical. Should field names be prettified for
#'   display?
#' @param xLabelAngle Numeric rotation angle for x-axis tick labels.
#' @param includeLegendBreaks Logical. Passed to `orderWmfmLegendValues()`.
#'
#' @return A `ggplot2` object.
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual labs theme_minimal theme element_text geom_segment
#' @export
plotWmfmExplanationClaimHeatmap = function(
    x,
    fieldColumns = NULL,
    naLabel = "(missing)",
    main = "Claim profile across repeated runs",
    xlab = NULL,
    ylab = "Run ID",
    prettyFieldLabels = TRUE,
    xLabelAngle = 45,
    includeLegendBreaks = FALSE
) {
  plotData = getWmfmRunsClaimProfileData(
    x = x,
    fieldColumns = fieldColumns,
    naLabel = naLabel,
    prettyFieldLabels = prettyFieldLabels
  )

  legendValues = unique(plotData$value)
  legendValues = orderWmfmLegendValues(
    legendValues,
    includeBreaks = includeLegendBreaks
  )
  legendValues = legendValues[nzchar(legendValues)]

  colourMap = getWmfmClaimColorMap()
  missingValues = setdiff(legendValues, names(colourMap))

  if (length(missingValues) > 0) {
    fallbackColours = gray.colors(length(missingValues), start = 0.85, end = 0.35)
    names(fallbackColours) = missingValues
    colourMap = c(colourMap, fallbackColours)
  }

  colourMap = colourMap[legendValues]
  legendLabels = makeWmfmLegendLabels(legendValues)

  missingDf = plotData[plotData$value == naLabel, , drop = FALSE]

  plotObj = ggplot(
    plotData,
    aes(
      x = fieldLabel,
      y = runId,
      fill = value
    )
  ) +
    geom_tile(colour = "white", linewidth = 0.4) +
    scale_fill_manual(
      values = colourMap,
      breaks = legendValues,
      labels = legendLabels,
      drop = FALSE
    ) +
    labs(
      title = main,
      x = xlab,
      y = ylab,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = xLabelAngle,
        vjust = 1,
        hjust = 1
      )
    )

  if (nrow(missingDf) > 0) {
    plotObj = plotObj +
      geom_segment(
        data = missingDf,
        aes(
          x = as.numeric(fieldLabel) - 0.42,
          xend = as.numeric(fieldLabel) + 0.42,
          y = as.numeric(runId) - 0.42,
          yend = as.numeric(runId) + 0.42
        ),
        inherit.aes = FALSE,
        linewidth = 0.4,
        colour = "#4D4D4D"
      ) +
      geom_segment(
        data = missingDf,
        aes(
          x = as.numeric(fieldLabel) - 0.42,
          xend = as.numeric(fieldLabel) + 0.42,
          y = as.numeric(runId) + 0.42,
          yend = as.numeric(runId) - 0.42
        ),
        inherit.aes = FALSE,
        linewidth = 0.4,
        colour = "#4D4D4D"
      )
  }

  plotObj
}
