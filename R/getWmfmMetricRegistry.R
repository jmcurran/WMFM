#' Return the WMFM metric registry
#'
#' Defines metric metadata used by WMFM score comparison, stability
#' summaries, and plotting. The registry is intended to be the single
#' source of truth for which metrics are available and how they should
#' be treated.
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
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
      "ordinal",
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
      "Main effect coverage adequate",
      "Reference group coverage adequate",
      "Clarity adequate",
      "Numeric expression adequate",
      "Comparison structure clear",
      "Fatal flaw detected",
      "Overall pass"
    ),
    group = c(
      "overall",
      "dimension",
      "dimension",
      "dimension",
      "dimension",
      "dimension",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "judgement",
      "outcome",
      "outcome"
    ),
    includeInComparison = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    includeInStability = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    includeInPlots = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),
    stringsAsFactors = FALSE
  )

  registry$orderedLevels = I(list(
    NULL,            # overallScore
    NULL,            # factualScore
    NULL,            # inferenceScore
    NULL,            # completenessScore
    NULL,            # clarityScore
    NULL,            # calibrationScore
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    c("0", "1", "2"),
    NULL,            # fatalFlawDetected
    NULL             # overallPass
  ))

  validateWmfmMetricRegistry(registry)

  registry
}
