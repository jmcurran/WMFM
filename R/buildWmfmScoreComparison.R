#' Build a `wmfmScoreComparison` object
#'
#' Internal helper that constructs agreement summaries and run-level
#' paired data for two score data frames.
#'
#' @param leftDf Long-form score data for the left method.
#' @param rightDf Long-form score data for the right method.
#' @param leftMethod Name of the left method.
#' @param rightMethod Name of the right method.
#' @param sourceLabel Character label describing the comparison source.
#' @param registry Metric registry.
#'
#' @return An object of class `wmfmScoreComparison`.
#' @keywords internal
buildWmfmScoreComparison = function(
  leftDf,
  rightDf,
  leftMethod,
  rightMethod,
  sourceLabel,
  registry = getWmfmMetricRegistry()
) {
  validateWmfmMetricRegistry(registry)

  pairData = buildWmfmComparisonPairs(
    leftDf = leftDf,
    rightDf = rightDf,
    registry = registry,
    leftMethod = leftMethod,
    rightMethod = rightMethod
  )

  registry = registry[registry$includeInComparison, , drop = FALSE]

  collect = function(metricType, summarizer) {
    rows = registry$metricType == metricType

    if (!any(rows)) {
      return(data.frame())
    }

    out = lapply(seq_len(nrow(registry[rows, , drop = FALSE])), function(i) {
      metricRow = registry[rows, , drop = FALSE][i, , drop = FALSE]
      metricName = metricRow$metricName
      leftName = metricName
      rightName = metricName

      if (!(leftName %in% names(leftDf) && rightName %in% names(rightDf))) {
        return(NULL)
      }

      merged = merge(
        leftDf[c("runId", leftName)],
        rightDf[c("runId", rightName)],
        by = "runId",
        suffixes = c(".x", ".y")
      )

      summarizer(
        leftVec = merged[[paste0(metricName, ".x")]],
        rightVec = merged[[paste0(metricName, ".y")]],
        metricRow = metricRow
      )
    })

    out = Filter(Negate(is.null), out)

    if (length(out) == 0) {
      return(data.frame())
    }

    do.call(rbind, out)
  }

  binaryAgreement = collect("binary", summarizeBinaryAgreement)
  ordinalAgreement = collect("ordinal", summarizeOrdinalAgreement)
  continuousAgreement = collect("continuous", summarizeContinuousAgreement)

  pairedOverallScores = data.frame()
  overallSummary = NULL

  if ("overallScore" %in% names(leftDf) && "overallScore" %in% names(rightDf)) {
    mergedOverall = merge(
      leftDf[c("runId", "overallScore")],
      rightDf[c("runId", "overallScore")],
      by = "runId",
      suffixes = c(".x", ".y")
    )

    l = as.numeric(mergedOverall$overallScore.x)
    r = as.numeric(mergedOverall$overallScore.y)
    ok = !(is.na(l) | is.na(r))

    pairedOverallScores = data.frame(
      runId = mergedOverall$runId[ok],
      leftOverallScore = l[ok],
      rightOverallScore = r[ok],
      meanOverallScore = (l[ok] + r[ok]) / 2,
      differenceOverallScore = r[ok] - l[ok],
      stringsAsFactors = FALSE
    )

    if (nrow(pairedOverallScores) > 0) {
      d = pairedOverallScores$differenceOverallScore
      md = mean(d)
      sdv = if (length(d) > 1) stats::sd(d) else NA_real_

      overallSummary = list(
        meanDifferenceRightMinusLeft = md,
        sdDifferenceRightMinusLeft = sdv,
        loaLower = md - 1.96 * sdv,
        loaUpper = md + 1.96 * sdv
      )
    }
  }

  structure(
    list(
      source = sourceLabel,
      leftMethod = leftMethod,
      rightMethod = rightMethod,
      registry = registry,
      binaryAgreement = binaryAgreement,
      ordinalAgreement = ordinalAgreement,
      continuousAgreement = continuousAgreement,
      pairData = pairData,
      overallSummary = overallSummary,
      pairedOverallScores = pairedOverallScores
    ),
    class = "wmfmScoreComparison"
  )
}
