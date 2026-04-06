#' Build metric-specific evidence summary for diagnosis
#'
#' Internal helper for \code{diagnose()} that creates a compact,
#' metric-aware evidence summary from the run-level diagnosis data.
#'
#' The initial implementation focuses on fields that are especially useful
#' for diagnosing disagreement on \code{numericExpressionAdequate}. For other
#' metrics, the function returns a lighter generic summary.
#'
#' @param metricDf Run-level metric diagnosis data.
#' @param metric Metric name.
#'
#' @return A one-row data frame containing metric-specific summary fields.
#'
#' @keywords internal
buildMetricEvidenceSummary = function(metricDf, metric) {
  nRuns = nrow(metricDf)

  if (nRuns < 1L) {
    return(data.frame(metric = metric, stringsAsFactors = FALSE))
  }

  countValue = function(x, value) {
    if (is.null(x)) {
      return(NA_integer_)
    }

    sum(as.character(x) == as.character(value), na.rm = TRUE)
  }

  propTrue = function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }

    mean(as.logical(x), na.rm = TRUE)
  }

  firstPresent = function(df, candidates) {
    hits = intersect(candidates, names(df))
    if (length(hits) < 1L) {
      return(NULL)
    }

    df[[hits[1L]]]
  }

  nDisagree = sum(metricDf$disagrees, na.rm = TRUE)

  out = data.frame(
    metric = metric,
    nRuns = nRuns,
    nDisagree = nDisagree,
    stringsAsFactors = FALSE
  )

  if (identical(metric, "numericExpressionAdequate")) {
    effectScaleClaim = firstPresent(metricDf, "effectScaleClaim")
    percentLanguageMention = firstPresent(metricDf, "percentLanguageMention")
    comparisonLanguageMention = firstPresent(metricDf, "comparisonLanguageMention")
    conditionalLanguageMention = firstPresent(metricDf, "conditionalLanguageMention")

    out$effectScale_notStated_n = countValue(effectScaleClaim, "not_stated")
    out$effectScale_mixedOrUnclear_n = countValue(effectScaleClaim, "mixed_or_unclear")
    out$effectScale_additive_n = countValue(effectScaleClaim, "additive")
    out$effectScale_multiplicative_n = countValue(effectScaleClaim, "multiplicative")
    out$percentLanguageMention_rate = propTrue(percentLanguageMention)
    out$comparisonLanguageMention_rate = propTrue(comparisonLanguageMention)
    out$conditionalLanguageMention_rate = propTrue(conditionalLanguageMention)

    likelyIssue = NA_character_

    if (nDisagree > 0L) {
      likelyIssue = "insufficientContext"

      if (!is.null(effectScaleClaim)) {
        notStatedRate = out$effectScale_notStated_n / nRuns
        mixedRate = out$effectScale_mixedOrUnclear_n / nRuns
        additiveRate = out$effectScale_additive_n / nRuns
      } else {
        notStatedRate = NA_real_
        mixedRate = NA_real_
        additiveRate = NA_real_
      }

      percentRate = out$percentLanguageMention_rate
      comparisonRate = out$comparisonLanguageMention_rate
      conditionalRate = out$conditionalLanguageMention_rate

      if (!is.na(notStatedRate) && notStatedRate >= 0.6) {
        likelyIssue = "effectScaleExtractionOftenMissing"
      } else if (!is.na(mixedRate) && mixedRate >= 0.6) {
        likelyIssue = "effectScaleExtractionOftenUnclear"
      } else if (!is.na(additiveRate) &&
                 additiveRate >= 0.4 &&
                 !is.na(percentRate) &&
                 percentRate < 0.2) {
        likelyIssue = "ruleMayUndervalueAdditiveNumericLanguage"
      } else if (!is.na(comparisonRate) &&
                 !is.na(conditionalRate) &&
                 (comparisonRate >= 0.5 || conditionalRate >= 0.5)) {
        likelyIssue = "numericContentPresentButRuleInputsMayMissIt"
      }
    }

    out$likelyIssue = likelyIssue
    return(out)
  }

  out$likelyIssue =
    if (nDisagree > 0L) {
      "metricSpecificEvidenceNotYetImplemented"
    } else {
      NA_character_
    }

  out
}
