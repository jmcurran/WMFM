#' Plot a disagreement heatmap for WMFM score comparison
#'
#' Draws a run-by-metric heatmap from the run-level paired comparison data in a
#' `wmfmScoreComparison` object. The fill indicates the degree of disagreement
#' between the left and right scoring methods.
#'
#' By default, the heatmap includes ordinal and binary metrics only. Continuous
#' metrics can be included by setting `includeContinuous = TRUE`, in which case
#' they are classified using absolute-difference bins.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param includeContinuous Logical. Should continuous metrics be included?
#' @param orderMetrics Character. One of `"worst"` or `"registry"`.
#' @param facetBy Character. One of `"group"`, `"metricType"`, or `"none"`.
#' @param continuousBreaks Numeric vector of length 3 giving cut points for the
#'   absolute difference bins used for continuous metrics.
#'
#' @return A `ggplot` object.
#' @keywords internal
plotWmfmScoreHeatmap = function(
    x,
    includeContinuous = FALSE,
    orderMetrics = c("worst", "registry"),
    facetBy = c("group", "metricType", "none"),
    continuousBreaks = c(0.10, 0.35, 0.75)
) {
  orderMetrics = match.arg(orderMetrics)
  facetBy = match.arg(facetBy)

  pairData = classifyWmfmScoreDisagreement(
    x = x,
    includeContinuous = includeContinuous,
    continuousBreaks = continuousBreaks
  )

  if (!is.data.frame(pairData) || nrow(pairData) == 0) {
    stop("No run-level pair data are available to plot.", call. = FALSE)
  }

  pairData = pairData[!is.na(pairData$disagreementClass), , drop = FALSE]

  if (nrow(pairData) == 0) {
    stop("No pair data remain after filtering for heatmapable metrics.", call. = FALSE)
  }

  if (!isTRUE(includeContinuous)) {
    pairData = pairData[pairData$metricType %in% c("binary", "ordinal"), , drop = FALSE]
  }

  if (nrow(pairData) == 0) {
    stop("No binary or ordinal pair data are available to plot.", call. = FALSE)
  }

  metricSummary = stats::aggregate(
    x = list(n = pairData$runId),
    by = list(
      metric = pairData$metric,
      label = pairData$label,
      group = pairData$group,
      metricType = pairData$metricType
    ),
    FUN = length
  )

  metricSummary$severity = 0

  severityWeights = c(
    exact = 0,
    adjacent = 1,
    small = 1,
    moderate = 2,
    different = 2,
    large = 3
  )

  for (i in seq_len(nrow(metricSummary))) {
    rows = pairData$metric == metricSummary$metric[i]
    classes = pairData$disagreementClass[rows]
    weights = severityWeights[classes]
    metricSummary$severity[i] = mean(weights, na.rm = TRUE)
  }

  registryOrder = x$registry$metricName
  registryLabels = x$registry$label
  registryOrderLookup = stats::setNames(seq_along(registryOrder), registryOrder)

  if (identical(orderMetrics, "worst")) {
    metricSummary = metricSummary[
      order(metricSummary$severity, decreasing = TRUE, metricSummary$label),
      ,
      drop = FALSE
    ]
  } else {
    metricSummary$registryIndex = registryOrderLookup[metricSummary$metric]
    metricSummary = metricSummary[
      order(metricSummary$registryIndex, metricSummary$label),
      ,
      drop = FALSE
    ]
  }

  pairData$label = factor(pairData$label, levels = rev(unique(metricSummary$label)))
  pairData$runId = factor(pairData$runId, levels = sort(unique(pairData$runId)))

  disagreementLevels = c("exact", "adjacent", "small", "moderate", "different", "large")
  pairData$disagreementClass = factor(
    pairData$disagreementClass,
    levels = disagreementLevels
  )

  palette = c(
    exact = "#D9D9D9",
    adjacent = "#80B1D3",
    small = "#B3DE69",
    moderate = "#FDB462",
    different = "#E7298A",
    large = "#D95F02"
  )

  keepLevels = levels(stats::na.omit(pairData$disagreementClass))
  palette = palette[intersect(names(palette), keepLevels)]

  p = ggplot2::ggplot(
    pairData,
    ggplot2::aes(x = runId, y = label, fill = disagreementClass)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_manual(values = palette, drop = TRUE) +
    ggplot2::labs(
      title = "Run-level disagreement heatmap",
      subtitle = paste0(
        x$rightMethod %||% "right",
        " versus ",
        x$leftMethod %||% "left"
      ),
      x = "Run",
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey95")
    )

  if (!identical(facetBy, "none")) {
    p = p + ggplot2::facet_grid(stats::as.formula(paste(facetBy, "~ .")), scales = "free_y", space = "free_y")
  }

  p
}
