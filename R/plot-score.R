#' Make deterministic category colours for WMFM heatmaps
#'
#' Generates a stable, perceptually stronger mapping from category values to
#' colours using only base R / grDevices. The same category value always maps
#' to the same colour, regardless of the order in which values appear.
#'
#' Colours are generated in HCL space using a simple deterministic string hash.
#' This gives better separation than short fixed palettes, especially when many
#' distinct values are present.
#'
#' @param values A vector of category values.
#' @param naLabel Character label used for missing values after coercion.
#' @return A named character vector of colours, where names are category values.
#' @examples
#' makeWmfmDeterministicCategoryColors(c("yes", "no", "mixed", "(missing)"))
#' @export
makeWmfmDeterministicCategoryColors = function(values,
                                               naLabel = "(missing)") {

  values = as.character(values)
  uniqueValues = sort(unique(values))

  if (length(uniqueValues) == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  hashString = function(x) {
    ints = utf8ToInt(enc2utf8(x))
    hash = 2166136261

    for (i in seq_along(ints)) {
      hash = bitwXor(hash, ints[i])
      hash = (hash * 16777619) %% 2147483647
    }

    as.integer(abs(hash))
  }

  makeColour = function(x) {
    if (identical(x, naLabel)) {
      return("#D9D9D9")
    }

    hash = hashString(x)

    hue = (hash %% 360) + (((hash %/% 360) %% 1000) / 1000)
    chromaOptions = c(55, 70, 85)
    luminanceOptions = c(42, 55, 68)

    chroma = chromaOptions[(hash %% length(chromaOptions)) + 1]
    luminance = luminanceOptions[((hash %/% 7) %% length(luminanceOptions)) + 1]

    grDevices::hcl(
      h = hue,
      c = chroma,
      l = luminance,
      fixup = TRUE
    )
  }

  colours = vapply(uniqueValues, makeColour, character(1), USE.NAMES = FALSE)

  duplicatedColours = duplicated(colours) | duplicated(colours, fromLast = TRUE)

  if (any(duplicatedColours)) {
    dupIdx = which(duplicatedColours)

    for (k in seq_along(dupIdx)) {
      i = dupIdx[k]

      if (identical(uniqueValues[i], naLabel)) {
        next
      }

      hash = hashString(uniqueValues[i]) + (k * 137)
      hue = (hash %% 360) + 0.5
      chromaOptions = c(55, 70, 85)
      luminanceOptions = c(42, 55, 68)

      chroma = chromaOptions[(hash %% length(chromaOptions)) + 1]
      luminance = luminanceOptions[((hash %/% 7) %% length(luminanceOptions)) + 1]

      colours[i] = grDevices::hcl(
        h = hue,
        c = chroma,
        l = luminance,
        fixup = TRUE
      )
    }
  }

  stats::setNames(colours, uniqueValues)
}
#' Plot a run-level disagreement heatmap for WMFM score comparisons
#'
#' Visualises run-level disagreement between two scoring methods stored in a
#' `wmfmScoreComparison` object. The heatmap is built from `pairData` and is
#' intended primarily for ordinal and binary metrics.
#'
#' Ordinal metrics are classified into disagreement classes using the absolute
#' difference between the paired scores:
#' \itemize{
#'   \item `exact` = no difference,
#'   \item `adjacent` = difference of 1,
#'   \item `moderate` = difference of 2 or more.
#' }
#'
#' Binary metrics are classified as either `exact` or `different`.
#'
#' Metrics can be ordered by mean disagreement severity so that the worst-agreeing
#' metrics appear first.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param includeMetricTypes Character vector of metric types to include.
#'   Defaults to `c("ordinal", "binary")`.
#' @param orderBy Character. One of `"worst"`, `"registry"`, or `"alphabetical"`.
#' @param facetBy Character. One of `"group"` or `"metricType"`.
#' @param prettyLabels Logical. Should metric labels be prettified using the
#'   stored labels in `pairData`?
#' @param showRunEvery Integer. Show every `k`th run label on the x-axis.
#' @param ... Unused. Included for future compatibility.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile facet_grid scale_fill_manual
#'   scale_x_continuous labs theme_minimal theme element_text element_blank
#'   element_rect margin guides guide_legend
plotWmfmScoreHeatmap = function(
    x,
    includeMetricTypes = c("ordinal", "binary"),
    orderBy = c("worst", "registry", "alphabetical"),
    facetBy = c("group", "metricType"),
    prettyLabels = TRUE,
    showRunEvery = 1L,
    ...
) {

  orderBy = match.arg(orderBy)
  facetBy = match.arg(facetBy)

  if (!inherits(x, "wmfmScoreComparison")) {
    stop("`x` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  pairData = x$pairData

  if (is.null(pairData) || !is.data.frame(pairData) || nrow(pairData) == 0) {
    stop("No run-level pair data are available to plot.", call. = FALSE)
  }

  requiredCols = c(
    "runId",
    "metric",
    "label",
    "group",
    "metricType",
    "leftValue",
    "rightValue"
  )

  missingCols = setdiff(requiredCols, names(pairData))

  if (length(missingCols) > 0) {
    stop(
      "`pairData` is missing required columns: ",
      paste(missingCols, collapse = ", "),
      call. = FALSE
    )
  }

  df = pairData[pairData$metricType %in% includeMetricTypes, , drop = FALSE]

  if (nrow(df) == 0) {
    stop("No compatible metric types are available to plot.", call. = FALSE)
  }

  classifyOne = function(metricType, leftValue, rightValue) {
    if (metricType == "binary") {
      leftLogical = as.logical(leftValue)
      rightLogical = as.logical(rightValue)

      if (is.na(leftLogical) || is.na(rightLogical)) {
        return(NA_character_)
      }

      if (identical(leftLogical, rightLogical)) {
        return("exact")
      }

      return("different")
    }

    if (metricType == "ordinal") {
      leftNum = suppressWarnings(as.numeric(leftValue))
      rightNum = suppressWarnings(as.numeric(rightValue))

      if (is.na(leftNum) || is.na(rightNum)) {
        return(NA_character_)
      }

      absDiff = abs(rightNum - leftNum)

      if (absDiff == 0) {
        return("exact")
      }

      if (absDiff == 1) {
        return("adjacent")
      }

      return("moderate")
    }

    NA_character_
  }

  df$disagreementClass = mapply(
    FUN = classifyOne,
    metricType = df$metricType,
    leftValue = df$leftValue,
    rightValue = df$rightValue,
    USE.NAMES = FALSE
  )

  df = df[!is.na(df$disagreementClass), , drop = FALSE]

  if (nrow(df) == 0) {
    stop("No non-missing disagreement values are available to plot.", call. = FALSE)
  }

  severityMap = c(
    exact = 0,
    adjacent = 1,
    moderate = 2,
    different = 3
  )

  df$severity = unname(severityMap[df$disagreementClass])

  metricMeta = unique(df[, c("metric", "label", "group", "metricType"), drop = FALSE])

  if (!isTRUE(prettyLabels)) {
    metricMeta$label = metricMeta$metric
  }

  if (orderBy == "worst") {
    metricScores = stats::aggregate(
      severity ~ metric,
      data = df,
      FUN = mean
    )

    metricMeta = merge(metricMeta, metricScores, by = "metric", all.x = TRUE, sort = FALSE)
    metricMeta = metricMeta[order(metricMeta$severity, metricMeta$label, decreasing = TRUE), , drop = FALSE]
  } else if (orderBy == "alphabetical") {
    metricMeta = metricMeta[order(metricMeta$label), , drop = FALSE]
  } else {
    registry = x$registry

    if (is.data.frame(registry) && nrow(registry) > 0 && "metricName" %in% names(registry)) {
      metricMeta$registryOrder = match(metricMeta$metric, registry$metricName)
      metricMeta = metricMeta[order(metricMeta$registryOrder, metricMeta$label), , drop = FALSE]
      metricMeta$registryOrder = NULL
    }
  }

  df$label = factor(df$label, levels = rev(metricMeta$label))
  df$runId = suppressWarnings(as.integer(df$runId))

  fillValues = c(
    exact = "#E0E0E0",
    adjacent = "#4E79A7",
    moderate = "#F28E2B",
    different = "#E15759"
  )

  legendBreaks = c("exact", "adjacent", "moderate", "different")
  legendLabels = c("Exact", "Off by 1", "Off by 2+", "Different")
  presentBreaks = legendBreaks[legendBreaks %in% unique(df$disagreementClass)]
  presentLabels = legendLabels[match(presentBreaks, legendBreaks)]

  runBreaks = sort(unique(df$runId))

  if (isTRUE(showRunEvery > 1L)) {
    runBreaks = runBreaks[seq(1, length(runBreaks), by = as.integer(showRunEvery))]
  }

  p = ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$runId,
      y = .data$label,
      fill = .data$disagreementClass
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.35) +
    ggplot2::facet_grid(
      stats::as.formula(paste(facetBy, "~ .")),
      scales = "free_y",
      space = "free_y"
    ) +
    ggplot2::scale_fill_manual(
      values = fillValues,
      breaks = presentBreaks,
      labels = presentLabels,
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = runBreaks,
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Run-level disagreement heatmap",
      subtitle = paste(x$rightMethod, "versus", x$leftMethod),
      x = "Run",
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = ggplot2::element_text(size = 10),
      strip.text.y = ggplot2::element_text(angle = 270, face = "bold"),
      strip.background = ggplot2::element_rect(fill = "#F0F0F0", colour = "#666666"),
      legend.position = "bottom",
      legend.key.height = grid::unit(0.8, "cm"),
      legend.key.width = grid::unit(1.2, "cm"),
      plot.title = ggplot2::element_text(face = "bold", size = 18),
      plot.subtitle = ggplot2::element_text(size = 13),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
    )

  p
}
#' Plot ordinal agreement summary for WMFM score comparison
#'
#' @param x A wmfmScoreComparison object
#' @param orderBy "worst" or "registry"
#'
#' @return ggplot object
#' @export
plotWmfmScoreAgreementSummary = function(x, orderBy = c("worst", "registry")) {

  orderBy = match.arg(orderBy)

  df = x$ordinalAgreement

  if (is.null(df) || nrow(df) == 0) {
    stop("No ordinal agreement summary is available to plot.", call. = FALSE)
  }

  # ---- ordering ----
  if (orderBy == "worst") {
    score =
      (1 - df$proportionAdjacent) +
      df$meanAbsoluteDifference

    ord = order(score, decreasing = TRUE)
    df = df[ord, , drop = FALSE]
  }

  df$label = factor(df$label, levels = rev(df$label))

  # ---- reshape ----
  agreementDf = data.frame(
    label = rep(df$label, 3),
    value = c(df$weightedKappa, df$proportionAdjacent, df$proportionEqual),
    statistic = factor(
      rep(c("Kappa", "Adjacent", "Exact"), each = nrow(df)),
      levels = c("Kappa", "Adjacent", "Exact")
    )
  )

  madDf = data.frame(
    label = df$label,
    value = df$meanAbsoluteDifference
  )

  # ---- agreement panel ----
  p1 = ggplot(agreementDf, aes(x = .data$value, y = .data$label, shape = .data$statistic)) +
    geom_point(size = 3) +
    scale_x_continuous(limits = c(0, 1)) +
    geom_vline(xintercept = 0.8, linetype = "dashed", alpha = 0.3) +
    labs(
      title = "Agreement metrics",
      x = NULL,
      y = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      plot.margin = margin(5, 20, 5, 10)
    )

  # ---- MAD panel ----
  p2 = ggplot(madDf, aes(x = .data$value, y = .data$label)) +
    geom_point(shape = 3, size = 4) +
    geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.3) +
    labs(
      title = "Mean absolute difference",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.y = element_blank(),
      plot.margin = margin(5, 10, 5, 10)
    )

  # ---- combine ----
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for this plot.", call. = FALSE)
  }

  p = p1 + p2 + patchwork::plot_layout(widths = c(2, 1))

  p + patchwork::plot_annotation(
    title = "Agreement across ordinal metrics",
    subtitle = paste(x$rightMethod, "versus", x$leftMethod)
  )
}

