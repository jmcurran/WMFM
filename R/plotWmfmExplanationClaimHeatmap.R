#' Plot a heatmap of semantic explanation claims across repeated WMFM runs
#'
#' Draws a run-by-claim heatmap to help assess whether repeated model
#' explanations differ in substantive meaning rather than only word choice.
#'
#' Rows represent runs and columns represent extracted semantic claim
#' variables. Each unique value within a claim column is assigned a colour.
#'
#' The input may be either:
#' \itemize{
#'   \item a data.frame of run records, or
#'   \item a list returned by `runWMFMPackageExampleRepeated()` containing a
#'   `runsDf` element.
#' }
#'
#' If `claimColumns = NULL`, the function uses a default set of semantic claim
#' columns when present.
#'
#' Colour assignment uses the named WMFM semantic colour map first. Any unseen
#' values are assigned colours deterministically from a fallback qualitative
#' palette so that the plot remains stable and readable.
#'
#' @param runsDf A data.frame of run records, or a list containing a
#'   data.frame named `runsDf`.
#' @param claimColumns Optional character vector giving the columns to plot.
#' @param runIdColumn Character. Column to use for row labels.
#' @param sortRows Logical. Should rows be sorted by claim pattern?
#' @param naLabel Character. Label used for missing values.
#' @param main Character. Plot title.
#' @param xlab Character. X-axis label. Defaults to `""`.
#' @param ylab Character. Y-axis label.
#' @param cexAxis Numeric. Axis text expansion factor.
#' @param cexLegend Numeric. Legend text expansion factor.
#' @param xLabelSrt Numeric. Rotation angle in degrees for x-axis tick labels.
#' @param mar Numeric vector of length 4 giving heatmap plot margins.
#' @param legendWidth Numeric. Relative width allocated to the legend panel.
#'
#' @return Invisibly returns a list describing the plotted data.
#' @examples
#' \dontrun{
#' plotWmfmExplanationClaimHeatmap(repeatedRuns)
#' }
#' @export
plotWmfmExplanationClaimHeatmap = function(
    runsDf,
    claimColumns = NULL,
    runIdColumn = "runId",
    sortRows = TRUE,
    naLabel = "(missing)",
    main = "WMFM explanation claim heatmap",
    xlab = "",
    ylab = "Run",
    cexAxis = 0.8,
    cexLegend = 0.85,
    xLabelSrt = 30,
    mar = c(10, 8, 4, 2),
    legendWidth = 2.8
) {

  extractRunsDf = function(x) {
    if (is.data.frame(x)) {
      return(x)
    }

    if (is.list(x) && "runsDf" %in% names(x) && is.data.frame(x$runsDf)) {
      return(x$runsDf)
    }

    stop(
      "`runsDf` must be a data.frame or a list containing a data.frame named `runsDf`.",
      call. = FALSE
    )
  }

  getDefaultClaimColumns = function(df) {
    preferred = c(
      "effectDirection",
      "effectScale",
      "mentionsReferenceGroup",
      "mentionsInteraction",
      "interactionClaim",
      "interactionInference",
      "uncertaintyMentioned",
      "usesInferentialLanguage",
      "usesDescriptiveOnlyLanguage",
      "overclaimDetected",
      "inferentialStyle",
      "mentionsConfidenceInterval",
      "usesPercentLanguage"
    )

    preferred[preferred %in% names(df)]
  }

  coerceClaimColumn = function(x, naLabel) {
    if (is.logical(x)) {
      return(ifelse(is.na(x), naLabel, ifelse(x, "TRUE", "FALSE")))
    }

    if (is.factor(x)) {
      x = as.character(x)
    }

    if (is.numeric(x)) {
      x = as.character(x)
    }

    x = as.character(x)
    x[is.na(x)] = naLabel
    x[trimws(x) == ""] = naLabel
    x
  }

  buildFallbackPaletteBank = function() {
    basePalette = c(
      "#000000", "#E69F00", "#56B4E9", "#0072B2", "#D55E00",
      "#CC79A7", "#A6CEE3", "#1F78B4", "#FB9A99", "#E31A1C",
      "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99",
      "#B15928", "#8DD3C7", "#80B1D3", "#FDB462", "#FCCDE5",
      "#BC80BD", "#FFED6F"
    )

    if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
      return(unique(basePalette))
    }

    brewerPalette = c(
      RColorBrewer::brewer.pal(8, "Dark2"),
      RColorBrewer::brewer.pal(8, "Set2"),
      RColorBrewer::brewer.pal(9, "Set1"),
      RColorBrewer::brewer.pal(8, "Accent"),
      RColorBrewer::brewer.pal(12, "Paired")
    )

    palette = unique(c(basePalette, brewerPalette))

    anchorOrder = c(
      "#000000", "#E31A1C", "#0072B2", "#FF7F00", "#6A3D9A", "#CC79A7",
      "#56B4E9", "#D55E00", "#1F78B4", "#A6CEE3", "#FB9A99", "#FDB462",
      "#CAB2D6", "#BC80BD", "#8DD3C7", "#80B1D3", "#FFFF99", "#FFED6F",
      "#B15928", "#FCCDE5"
    )

    ordered = c(anchorOrder[anchorOrder %in% palette], palette[!palette %in% anchorOrder])
    unique(ordered)
  }

  buildColourKey = function(values, naLabel) {
    values = as.character(as.vector(values))
    uniqueValues = sort(unique(values))

    if (length(uniqueValues) == 0) {
      return(stats::setNames(character(0), character(0)))
    }

    defaultMap = getWmfmClaimColorMap()

    if (!naLabel %in% names(defaultMap)) {
      defaultMap = c(defaultMap, stats::setNames("#D9D9D9", naLabel))
    }

    matchedNames = intersect(uniqueValues, names(defaultMap))
    colourKey = defaultMap[matchedNames]

    unmatchedValues = setdiff(uniqueValues, names(defaultMap))

    if (length(unmatchedValues) == 0) {
      return(colourKey[uniqueValues])
    }

    hashString = function(x) {
      ints = utf8ToInt(enc2utf8(x))

      if (length(ints) == 0) {
        return(1L)
      }

      hash = 0

      for (i in seq_along(ints)) {
        hash = (hash * 131 + ints[i]) %% 2147483647
      }

      as.integer(hash + 1)
    }

    paletteBank = buildFallbackPaletteBank()
    usedColours = unname(colourKey)
    availablePalette = setdiff(paletteBank, usedColours)

    if (length(availablePalette) == 0) {
      availablePalette = paletteBank
    }

    nPalette = length(availablePalette)
    fallbackColours = rep(NA_character_, length(unmatchedValues))
    usedSlots = integer(0)

    for (i in seq_along(unmatchedValues)) {
      value = unmatchedValues[i]
      hash = hashString(value)
      slot = (hash %% nPalette) + 1L

      if (slot %in% usedSlots) {
        step = 11L

        while (slot %in% usedSlots) {
          slot = ((slot - 1L + step) %% nPalette) + 1L
        }
      }

      usedSlots = c(usedSlots, slot)
      fallbackColours[i] = availablePalette[slot]
    }

    fallbackKey = stats::setNames(fallbackColours, unmatchedValues)
    fullKey = c(colourKey, fallbackKey)

    fullKey[uniqueValues]
  }

  runsDf = extractRunsDf(runsDf)

  if (is.null(claimColumns)) {
    claimColumns = getDefaultClaimColumns(runsDf)
  }

  if (!is.character(claimColumns) || length(claimColumns) == 0) {
    stop(
      "No claim columns were supplied or detected. Please provide `claimColumns`.",
      call. = FALSE
    )
  }

  missingColumns = setdiff(claimColumns, names(runsDf))

  if (length(missingColumns) > 0) {
    stop(
      "The following `claimColumns` are not present in `runsDf`: ",
      paste(missingColumns, collapse = ", "),
      call. = FALSE
    )
  }

  plotDf = runsDf[, claimColumns, drop = FALSE]

  claimMatrix = do.call(
    cbind,
    lapply(plotDf, coerceClaimColumn, naLabel = naLabel)
  )

  claimMatrix = as.matrix(claimMatrix)
  colnames(claimMatrix) = claimColumns

  if (runIdColumn %in% names(runsDf)) {
    rowLabels = as.character(runsDf[[runIdColumn]])
  } else {
    rowLabels = as.character(seq_len(nrow(runsDf)))
  }

  if (isTRUE(sortRows) && nrow(claimMatrix) > 1) {
    orderDf = as.data.frame(claimMatrix, stringsAsFactors = FALSE)
    rowOrder = do.call(order, c(orderDf, list(na.last = TRUE)))
    claimMatrix = claimMatrix[rowOrder, , drop = FALSE]
    rowLabels = rowLabels[rowOrder]
  } else {
    rowOrder = seq_len(nrow(claimMatrix))
  }

  colourKey = buildColourKey(claimMatrix, naLabel = naLabel)

  colourMatrix = matrix(
    match(claimMatrix, names(colourKey)),
    nrow = nrow(claimMatrix),
    ncol = ncol(claimMatrix)
  )

  oldPar = graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldPar), add = TRUE)

  graphics::layout(
    matrix(c(1, 2), nrow = 1),
    widths = c(5, legendWidth)
  )

  graphics::par(mar = mar, xpd = FALSE)

  z = t(colourMatrix[nrow(colourMatrix):1, , drop = FALSE])

  graphics::image(
    x = seq_len(ncol(colourMatrix)),
    y = seq_len(nrow(colourMatrix)),
    z = z,
    col = unname(colourKey),
    axes = FALSE,
    xlab = xlab,
    ylab = ylab,
    main = main
  )

  graphics::axis(
    side = 2,
    at = seq_len(nrow(claimMatrix)),
    labels = rev(rowLabels),
    las = 2,
    cex.axis = cexAxis
  )

  xPositions = seq_len(ncol(claimMatrix))
  yBottom = graphics::par("usr")[3]

  graphics::axis(
    side = 1,
    at = xPositions,
    labels = FALSE,
    tck = -0.02
  )

  graphics::text(
    x = xPositions,
    y = yBottom - 0.35,
    labels = colnames(claimMatrix),
    srt = xLabelSrt,
    adj = 1,
    xpd = TRUE,
    cex = cexAxis
  )

  graphics::box()

  legendValues = orderWmfmLegendValues(
    names(colourKey),
    includeBreaks = TRUE
  )

  legendFill = rep(NA_character_, length(legendValues))
  nonBlank = legendValues != ""

  legendFill[nonBlank] = unname(colourKey[legendValues[nonBlank]])
  legendFill[!nonBlank] = NA

  graphics::par(mar = c(4, 0, 4, 1), xpd = NA)
  graphics::plot.new()

  graphics::legend(
    "topleft",
    legend = makeWmfmLegendLabels(legendValues),
    fill = legendFill,
    border = ifelse(is.na(legendFill), NA, "grey30"),
    bty = "n",
    cex = cexLegend,
    title = "Claim values",
    y.intersp = 1.1
  )

  invisible(list(
    plotData = claimMatrix,
    colourMatrix = colourMatrix,
    colourKey = colourKey,
    rowOrder = rowOrder,
    claimColumns = claimColumns
  ))
}
