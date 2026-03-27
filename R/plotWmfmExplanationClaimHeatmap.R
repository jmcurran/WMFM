#' Plot a heatmap of WMFM repeated-run explanation fields
#'
#' Draws a run-by-field heatmap for repeated WMFM explanation runs. The function
#' is designed for the revised repeated-run evaluation schema and can plot
#' extracted claim fields, judged quality fields, or aggregate scores.
#'
#' The input may be either:
#' \itemize{
#'   \item a data.frame of run records, or
#'   \item a list containing a `runsDf` element.
#' }
#'
#' When `fieldColumns = NULL`, a sensible default field set is selected using
#' the revised schema first and older field names as fallbacks where possible.
#'
#' Heatmaps are most useful for categorical or low-cardinality fields. Numeric
#' score columns are converted to character labels before plotting, so scores are
#' usually best shown only when they take a small set of rounded values.
#'
#' @param runsDf A data.frame of run records, or a list containing a data.frame
#'   named `runsDf`.
#' @param fieldColumns Optional character vector of columns to plot. When this
#'   is supplied it takes precedence over `plotType`.
#' @param plotType Character. One of `"claims"`, `"judged"`, `"scores"`, or
#'   `"auto"`.
#' @param runIdColumn Character. Column to use for row labels.
#' @param sortRows Logical. Should rows be sorted by field pattern?
#' @param naLabel Character. Label used for missing values.
#' @param main Character. Plot title.
#' @param xlab Character. X-axis label.
#' @param ylab Character. Y-axis label.
#' @param cexAxis Numeric. Axis text expansion factor.
#' @param cexLegend Numeric. Legend text expansion factor.
#' @param xLabelSrt Numeric. Rotation angle in degrees for x-axis tick labels.
#' @param mar Numeric vector of length 4 giving heatmap plot margins.
#' @param legendWidth Numeric. Relative width allocated to the legend panel.
#' @param prettyFieldLabels Logical. Should field names be prettified for axis
#'   labels?
#' @param includeBreaksInLegend Logical. Should the legend insert blank spacer
#'   rows between semantic groups?
#'
#' @return Invisibly returns a list describing the plotted data.
#' @examples
#' \dontrun{
#' plotWmfmExplanationClaimHeatmap(repeatedRuns)
#' plotWmfmExplanationClaimHeatmap(repeatedRuns, plotType = "judged")
#' }
#' @export
plotWmfmExplanationClaimHeatmap = function(
    runsDf,
    fieldColumns = NULL,
    plotType = c("claims", "judged", "scores", "auto"),
    runIdColumn = "runId",
    sortRows = TRUE,
    naLabel = "(missing)",
    main = NULL,
    xlab = "",
    ylab = "Run",
    cexAxis = 0.8,
    cexLegend = 0.85,
    xLabelSrt = 30,
    mar = c(10, 8, 4, 2),
    legendWidth = 2.8,
    prettyFieldLabels = TRUE,
    includeBreaksInLegend = TRUE
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

  firstPresent = function(df, candidates) {
    present = candidates[candidates %in% names(df)]

    if (length(present) == 0) {
      return(character(0))
    }

    present[1]
  }

  resolveAliasSet = function(df, aliasGroups) {
    out = character(0)

    for (i in seq_along(aliasGroups)) {
      matchName = firstPresent(df, aliasGroups[[i]])

      if (length(matchName) == 1) {
        out = c(out, matchName)
      }
    }

    unique(out)
  }

  getDefaultFieldColumns = function(df, plotType) {
    claimAliases = list(
      c("effectDirectionClaim", "effectDirection"),
      c("effectScaleClaim", "effectScale"),
      c("referenceGroupMention", "mentionsReferenceGroup"),
      c("interactionMention", "mentionsInteraction"),
      c("interactionSubstantiveClaim", "interactionClaim", "interactionDirection"),
      c("uncertaintyMention", "uncertaintyMentioned"),
      c("uncertaintyTypeClaim"),
      c("usesInferentialLanguage"),
      c("usesDescriptiveOnlyLanguage"),
      c("overclaimDetected"),
      c("underclaimDetected"),
      c("inferentialRegister", "inferentialStyle"),
      c("ciMention", "mentionsConfidenceInterval"),
      c("percentLanguageMention", "usesPercentLanguage"),
      c("conditionalLanguageMention"),
      c("comparisonLanguageMention"),
      c("outcomeMention"),
      c("predictorMention")
    )

    judgedAliases = list(
      c("interactionEvidenceAppropriate", "interactionInference"),
      c("effectDirectionCorrect"),
      c("effectScaleAppropriate"),
      c("referenceGroupHandledCorrectly"),
      c("interactionCoverageAdequate"),
      c("interactionSubstantiveCorrect"),
      c("uncertaintyHandlingAppropriate"),
      c("inferentialRegisterAppropriate"),
      c("mainEffectCoverageAdequate"),
      c("referenceGroupCoverageAdequate"),
      c("clarityAdequate"),
      c("numericExpressionAdequate"),
      c("comparisonStructureClear"),
      c("fatalFlawDetected"),
      c("overallPass")
    )

    scoreAliases = list(
      c("factualScore"),
      c("inferenceScore"),
      c("completenessScore"),
      c("clarityScore"),
      c("calibrationScore"),
      c("overallScore")
    )

    if (plotType == "claims") {
      return(resolveAliasSet(df, claimAliases))
    }

    if (plotType == "judged") {
      return(resolveAliasSet(df, judgedAliases))
    }

    if (plotType == "scores") {
      return(resolveAliasSet(df, scoreAliases))
    }

    claims = resolveAliasSet(df, claimAliases)

    if (length(claims) > 0) {
      return(claims)
    }

    judged = resolveAliasSet(df, judgedAliases)

    if (length(judged) > 0) {
      return(judged)
    }

    resolveAliasSet(df, scoreAliases)
  }

  coerceFieldColumn = function(x, naLabel) {
    if (is.logical(x)) {
      out = ifelse(is.na(x), naLabel, ifelse(x, "TRUE", "FALSE"))
      return(out)
    }

    if (is.factor(x)) {
      x = as.character(x)
    }

    if (is.numeric(x)) {
      if (all(is.na(x) | abs(x - round(x)) < .Machine$double.eps^0.5)) {
        x = as.character(as.integer(round(x)))
      } else {
        x = format(round(x, 2), trim = TRUE, nsmall = 0)
      }
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

    unique(c(basePalette, brewerPalette))
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

  orderLegendValues = function(values, includeBreaks = TRUE) {
    values = unique(values)

    groups = list(
      c("TRUE", "FALSE"),
      c("increase", "decrease", "mixed_or_both", "mixed_or_unclear", "not_stated"),
      c("additive", "multiplicative", "probability_or_odds", "mixed_or_unclear", "not_stated"),
      c(
        "difference_claimed", "difference_claimed_cautiously",
        "difference_claimed_strongly", "no_clear_difference",
        "appropriate", "adequate", "too_weak", "too_strong"
      ),
      c(
        "inferential", "descriptive_only", "descriptive",
        "generic_uncertainty", "confidence_interval"
      ),
      c("0", "1", "2"),
      c("pass", "fail", "fatal"),
      c("unclear", "mixed", "mixed_or_unclear", "not_applicable", "not_mentioned", "none", "(missing)", "missing")
    )

    ordered = character(0)

    for (group in groups) {
      inGroup = group[group %in% values & !group %in% ordered]

      if (length(inGroup) > 0) {
        ordered = c(ordered, inGroup)

        if (isTRUE(includeBreaks)) {
          ordered = c(ordered, "")
        }
      }
    }

    remaining = sort(setdiff(values, ordered))
    ordered = c(ordered, remaining)

    if (isTRUE(includeBreaks)) {
      while (length(ordered) > 0 && tail(ordered, 1) == "") {
        ordered = ordered[-length(ordered)]
      }
    }

    ordered
  }

  makeLegendLabels = function(values) {
    labels = values
    labels[labels == ""] = " "
    labels = gsub("_", " ", labels, fixed = TRUE)
    labels
  }

  prettifyFieldLabels = function(x) {
    prettyMap = c(
      effectDirectionClaim = "Direction claim",
      effectDirection = "Direction claim",
      effectScaleClaim = "Scale claim",
      effectScale = "Scale claim",
      referenceGroupMention = "Reference mention",
      mentionsReferenceGroup = "Reference mention",
      interactionMention = "Interaction mention",
      mentionsInteraction = "Interaction mention",
      interactionSubstantiveClaim = "Interaction claim",
      interactionClaim = "Interaction claim",
      interactionDirection = "Interaction claim",
      uncertaintyMention = "Uncertainty mention",
      uncertaintyMentioned = "Uncertainty mention",
      uncertaintyTypeClaim = "Uncertainty type",
      usesInferentialLanguage = "Inferential words",
      usesDescriptiveOnlyLanguage = "Descriptive-only words",
      overclaimDetected = "Overclaim",
      underclaimDetected = "Underclaim",
      inferentialRegister = "Register",
      inferentialStyle = "Register",
      ciMention = "CI mention",
      mentionsConfidenceInterval = "CI mention",
      percentLanguageMention = "% language",
      usesPercentLanguage = "% language",
      conditionalLanguageMention = "Conditional language",
      comparisonLanguageMention = "Comparison language",
      outcomeMention = "Outcome mention",
      predictorMention = "Predictor mention",
      interactionEvidenceAppropriate = "Interaction evidence",
      interactionInference = "Interaction evidence",
      effectDirectionCorrect = "Direction correct",
      effectScaleAppropriate = "Scale appropriate",
      referenceGroupHandledCorrectly = "Reference correct",
      interactionCoverageAdequate = "Interaction coverage",
      interactionSubstantiveCorrect = "Interaction correct",
      uncertaintyHandlingAppropriate = "Uncertainty handling",
      inferentialRegisterAppropriate = "Register appropriate",
      mainEffectCoverageAdequate = "Main-effect coverage",
      referenceGroupCoverageAdequate = "Reference coverage",
      clarityAdequate = "Clarity",
      numericExpressionAdequate = "Numbers",
      comparisonStructureClear = "Comparison structure",
      fatalFlawDetected = "Fatal flaw",
      overallPass = "Overall pass",
      factualScore = "Factual score",
      inferenceScore = "Inference score",
      completenessScore = "Completeness score",
      clarityScore = "Clarity score",
      calibrationScore = "Calibration score",
      overallScore = "Overall score"
    )

    out = x
    matched = intersect(names(prettyMap), x)
    out[match(matched, x)] = prettyMap[matched]
    out
  }

  plotType = match.arg(plotType)
  runsDf = extractRunsDf(runsDf)

  if (is.null(fieldColumns)) {
    fieldColumns = getDefaultFieldColumns(runsDf, plotType = plotType)
  }

  if (!is.character(fieldColumns) || length(fieldColumns) == 0) {
    stop(
      "No fields were supplied or detected. Please provide `fieldColumns`.",
      call. = FALSE
    )
  }

  missingColumns = setdiff(fieldColumns, names(runsDf))

  if (length(missingColumns) > 0) {
    stop(
      "The following `fieldColumns` are not present in `runsDf`: ",
      paste(missingColumns, collapse = ", "),
      call. = FALSE
    )
  }

  plotDf = runsDf[, fieldColumns, drop = FALSE]
  fieldMatrix = do.call(cbind, lapply(plotDf, coerceFieldColumn, naLabel = naLabel))
  fieldMatrix = as.matrix(fieldMatrix)
  colnames(fieldMatrix) = fieldColumns

  if (runIdColumn %in% names(runsDf)) {
    rowLabels = as.character(runsDf[[runIdColumn]])
  } else {
    rowLabels = as.character(seq_len(nrow(runsDf)))
  }

  if (isTRUE(sortRows) && nrow(fieldMatrix) > 1) {
    orderDf = as.data.frame(fieldMatrix, stringsAsFactors = FALSE)
    rowOrder = do.call(order, c(orderDf, list(na.last = TRUE)))
    fieldMatrix = fieldMatrix[rowOrder, , drop = FALSE]
    rowLabels = rowLabels[rowOrder]
  } else {
    rowOrder = seq_len(nrow(fieldMatrix))
  }

  colourKey = buildColourKey(fieldMatrix, naLabel = naLabel)
  colourMatrix = matrix(
    match(fieldMatrix, names(colourKey)),
    nrow = nrow(fieldMatrix),
    ncol = ncol(fieldMatrix)
  )

  if (is.null(main)) {
    main = switch(
      plotType,
      claims = "WMFM explanation claim heatmap",
      judged = "WMFM explanation quality heatmap",
      scores = "WMFM explanation score heatmap",
      auto = "WMFM explanation heatmap"
    )
  }

  xLabels = colnames(fieldMatrix)

  if (isTRUE(prettyFieldLabels)) {
    xLabels = prettifyFieldLabels(xLabels)
  }

  oldPar = graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldPar), add = TRUE)

  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(5, legendWidth))
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
    at = seq_len(nrow(fieldMatrix)),
    labels = rev(rowLabels),
    las = 2,
    cex.axis = cexAxis
  )

  xPositions = seq_len(ncol(fieldMatrix))
  yBottom = graphics::par("usr")[3]

  graphics::axis(side = 1, at = xPositions, labels = FALSE, tck = -0.02)
  graphics::text(
    x = xPositions,
    y = yBottom - 0.35,
    labels = xLabels,
    srt = xLabelSrt,
    adj = 1,
    xpd = TRUE,
    cex = cexAxis
  )

  graphics::box()

  legendValues = orderLegendValues(names(colourKey), includeBreaks = includeBreaksInLegend)
  legendFill = rep(NA_character_, length(legendValues))
  nonBlank = legendValues != ""
  legendFill[nonBlank] = unname(colourKey[legendValues[nonBlank]])
  legendFill[!nonBlank] = NA

  graphics::par(mar = c(4, 0, 4, 1), xpd = NA)
  graphics::plot.new()
  graphics::legend(
    "topleft",
    legend = makeLegendLabels(legendValues),
    fill = legendFill,
    border = ifelse(is.na(legendFill), NA, "grey30"),
    bty = "n",
    cex = cexLegend,
    title = "Field values",
    y.intersp = 1.1
  )

  invisible(list(
    plotData = fieldMatrix,
    colourMatrix = colourMatrix,
    colourKey = colourKey,
    rowOrder = rowOrder,
    fieldColumns = fieldColumns,
    plotType = plotType
  ))
}
