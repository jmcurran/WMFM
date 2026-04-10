#' Get the default WMFM claim colour map
#'
#' Returns a named character vector of colours for semantic values used in WMFM
#' raw repeated-run claim-profile heatmaps.
#'
#' The palette is designed for raw extracted claim fields and aims to keep
#' semantically different values visually distinct.
#'
#' @return A named character vector of hexadecimal colours.
#' @examples
#' getWmfmClaimColorMap()
#' @export
getWmfmClaimColorMap = function() {
  c(
    # --- Binary / flags ---
    "TRUE" = "#009E73",
    "FALSE" = "#D55E00",

    # --- Missingness / structural values ---
    "(missing)" = "#FFFFFF",
    "missing" = "#FFFFFF",
    "none" = "#8F8F8F",
    "not_applicable" = "#D7D7D7",
    "not_mentioned" = "#A6A6A6",
    "not_stated" = "#4D4D4D",

    # --- Direction / scale claims ---
    "increase" = "#4DAF4A",
    "decrease" = "#E66101",
    "mixed_or_both" = "#CC79A7",
    "mixed_or_unclear" = "#8073AC",
    "mixed" = "#6A51A3",
    "unclear" = "#984EA3",
    "additive" = "#7CAE00",
    "multiplicative" = "#1F78B4",
    "probability_or_odds" = "#08519C",

    # --- Interaction substantive claims ---
    "difference_claimed" = "#4292C6",
    "difference_claimed_cautiously" = "#6BAED6",
    "difference_claimed_strongly" = "#084594",
    "no_clear_difference" = "#E6AB02",

    # --- Register / uncertainty ---
    "inferential" = "#2171B5",
    "descriptive_only" = "#66A61E",
    "descriptive" = "#66A61E",
    "generic_uncertainty" = "#E6AB02",
    "confidence_interval" = "#084594",

    # --- Legacy ordinal values, kept in case they appear ---
    "0" = "#E41A1C",
    "1" = "#FDBF6F",
    "2" = "#1B9E77"
  )
}
#' Plot a raw-claim heatmap for WMFM runs
#'
#' Draws a run-by-field heatmap for a `wmfmRuns` object using raw extracted
#' claim fields only. Rows represent runs and columns represent claim fields.
#'
#' @param x A `wmfmRuns` object.
#' @param fieldColumns Optional character vector of raw claim fields to plot.
#' @param naLabel Character label used for missing values.
#' @param main Character plot title.
#' @param xlab Character x-axis label.
#' @param ylab Character y-axis label.
#' @param prettyFieldLabels Logical. Should field names be prettified for
#'   display?
#' @param fieldOrder Character. One of `"semantic"` or `"purity"`.
#' @param xLabelAngle Numeric rotation angle for x-axis tick labels.
#' @param includeLegendBreaks Logical. Passed to `orderWmfmLegendValues()`.
#'
#' @return A `ggplot2` object.
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual labs theme_minimal theme element_text geom_segment
#' @importFrom grDevices gray.colors
#' @export
plotWmfmExplanationClaimHeatmap = function(
    x,
    fieldColumns = NULL,
    naLabel = "(missing)",
    main = "Claim profile across repeated runs",
    xlab = NULL,
    ylab = "Run ID",
    prettyFieldLabels = TRUE,
    fieldOrder = c("semantic", "purity"),
    xLabelAngle = 45,
    includeLegendBreaks = FALSE
) {
  fieldOrder = match.arg(fieldOrder)

  plotData = getWmfmRunsClaimProfileData(
    x = x,
    fieldColumns = fieldColumns,
    naLabel = naLabel,
    prettyFieldLabels = prettyFieldLabels,
    fieldOrder = fieldOrder
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
      x = .data$fieldLabel,
      y = .data$runId,
      fill = .data$value
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
          x = as.numeric(.data$fieldLabel) - 0.42,
          xend = as.numeric(.data$fieldLabel) + 0.42,
          y = as.numeric(.data$runId) - 0.42,
          yend = as.numeric(.data$runId) + 0.42
        ),
        inherit.aes = FALSE,
        linewidth = 0.4,
        colour = "#4D4D4D"
      ) +
      geom_segment(
        data = missingDf,
        aes(
          x = as.numeric(.data$fieldLabel) - 0.42,
          xend = as.numeric(.data$fieldLabel) + 0.42,
          y = as.numeric(.data$runId) + 0.42,
          yend = as.numeric(.data$runId) - 0.42
        ),
        inherit.aes = FALSE,
        linewidth = 0.4,
        colour = "#4D4D4D"
      )
  }

  plotObj
}

