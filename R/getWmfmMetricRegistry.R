#' Return the WMFM metric registry
#'
#' Defines metric metadata used by WMFM score comparison, stability
#' summaries, and plotting. The registry is intended to be the single
#' source of truth for which metrics are available and how they should
#' be treated.
#'
#' The default registry below reflects the current WMFM scoring schema:
#' ordinal judged fields are stored as integer-like values \code{0},
#' \code{1}, and \code{2}; binary judged fields are stored as logicals;
#' and aggregate score fields are numeric.
#'
#' @return A data frame containing metric metadata.
#' @export
getWmfmMetricRegistry = function() {
  registry = data.frame(
    metricName = c(
      "overallScore",
      "factualScore",
      "inferenceScore",
      "completenessScore",
      "clarityScore",
      "calibrationScore",
      "effectDirectionCorrect",
      "effectScaleAppropriate",
      "referenceGroupHandledCorrectly",
      "interactionCoverageAdequate",
      "interactionSubstantiveCorrect",
      "uncertaintyHandlingAppropriate",
      "inferentialRegisterAppropriate",
      "mainEffectCoverageAdequate",
      "referenceGroupCoverageAdequate",
      "clarityAdequate",
      "numericExpressionAdequate",
      "comparisonStructureClear",
      "fatalFlawDetected",
      "overallPass"
    ),
    metricType = c(
      "continuous",
      "continuous",
      "continuous",
      "continuous",
      "continuous",
      "continuous",
      rep("ordinal", 11),
      "binary",
      "binary"
    ),
    label = c(
      "Overall score",
      "Factual score",
      "Inference score",
      "Completeness score",
      "Clarity score",
      "Calibration score",
      "Effect direction correct",
      "Effect scale appropriate",
      "Reference group handled correctly",
      "Interaction coverage adequate",
      "Interaction substantive correct",
      "Uncertainty handling appropriate",
      "Inferential register appropriate",
      "Main-effect coverage adequate",
      "Reference group coverage adequate",
      "Clarity adequate",
      "Numeric expression adequate",
      "Comparison structure clear",
      "Fatal flaw detected",
      "Overall pass"
    ),
    group = c(
      "overall",
      "dimension_scores",
      "dimension_scores",
      "dimension_scores",
      "dimension_scores",
      "dimension_scores",
      "interpretation",
      "interpretation",
      "reference",
      "interaction",
      "interaction",
      "inference",
      "inference",
      "completeness",
      "reference",
      "clarity",
      "clarity",
      "clarity",
      "quality_flags",
      "quality_flags"
    ),
    includeInComparison = rep(TRUE, 20),
    includeInStability = rep(TRUE, 20),
    includeInPlots = rep(TRUE, 20),
    stringsAsFactors = FALSE
  )

  ordinalLevels = c("0", "1", "2")

  registry$orderedLevels = I(list(
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    ordinalLevels,
    NULL,
    NULL
  ))

  validateWmfmMetricRegistry(registry)

  registry
}
