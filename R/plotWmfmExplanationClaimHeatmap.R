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
#' @param runsDf A data.frame of run records, or a list containing a
#'   data.frame named `runsDf`.
#' @param claimColumns Optional character vector giving the columns to plot.
#' @param runIdColumn Character. Column to use for row labels.
#' @param sortRows Logical. Should rows be sorted by claim pattern?
#' @param naLabel Character. Label used for missing values.
#' @param main Character. Plot title.
#' @param xlab Character. X-axis label.
#' @param ylab Character. Y-axis label.
#' @param cexAxis Numeric. Axis text expansion factor.
#' @param cexLegend Numeric. Legend text expansion factor.
#' @param mar Numeric vector of length 4 giving plot margins.
#' @param legendRightInset Numeric. Right-side inset used for the legend.
#'
#' @return Invisibly returns a list describing the plotted data.
#' @export
plotWmfmExplanationClaimHeatmap = function(
    runsDf,
    claimColumns = NULL,
    runIdColumn = "runId",
    sortRows = TRUE,
    naLabel = "(missing)",
    main = "WMFM explanation claim heatmap",
    xlab = "Claim",
    ylab = "Run",
    cexAxis = 0.8,
    cexLegend = 0.8,
    mar = c(8, 8, 4, 12),
    legendRightInset = -0.28
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
      "interactionDirection",
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

  buildColourKey = function(values) {
    uniqueValues = sort(unique(as.vector(values)))
    nValues = length(uniqueValues)

    if (nValues <= 8) {
      paletteValues = c(
        "#1b9e77", "#d95f02", "#7570b3", "#e7298a",
        "#66a61e", "#e6ab02", "#a6761d", "#666666"
      )
    } else if (nValues <= 12) {
      paletteValues = grDevices::hcl.colors(nValues, "Set 3")
    } else {
      paletteValues = grDevices::hcl.colors(nValues, "Dynamic")
    }

    stats::setNames(paletteValues[seq_len(nValues)], uniqueValues)
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

  colourKey = buildColourKey(claimMatrix)

  colourMatrix = matrix(
    match(claimMatrix, names(colourKey)),
    nrow = nrow(claimMatrix),
    ncol = ncol(claimMatrix)
  )

  oldPar = graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldPar), add = TRUE)

  graphics::par(mar = mar, xpd = NA)

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
    side = 1,
    at = seq_len(ncol(claimMatrix)),
    labels = colnames(claimMatrix),
    las = 2,
    cex.axis = cexAxis
  )

  graphics::axis(
    side = 2,
    at = seq_len(nrow(claimMatrix)),
    labels = rev(rowLabels),
    las = 2,
    cex.axis = cexAxis
  )

  graphics::box()

  graphics::legend(
    "topright",
    inset = c(legendRightInset, 0),
    legend = names(colourKey),
    fill = unname(colourKey),
    border = "grey30",
    bty = "n",
    cex = cexLegend,
    title = "Claim values"
  )

  invisible(list(
    plotData = claimMatrix,
    colourMatrix = colourMatrix,
    colourKey = colourKey,
    rowOrder = rowOrder,
    claimColumns = claimColumns
  ))
}
