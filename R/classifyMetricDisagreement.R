#' Classify likely source of metric disagreement
#'
#' Internal heuristic classifier for \code{diagnose()}.
#'
#' @param summaryDf One-row summary data frame produced by
#'   \code{buildMetricDiagnosisSummary()}.
#'
#' @return A single character string describing the likely disagreement
#'   class.
#'
#' @keywords internal
classifyMetricDisagreement = function(summaryDf) {
  stopifnot(is.data.frame(summaryDf), nrow(summaryDf) == 1L)

  disagreementRate = summaryDf$disagreementRate[[1L]]
  directionConsistency = summaryDf$directionConsistency[[1L]]

  detConstant = isTRUE(summaryDf$detConstant[[1L]])
  llmConstant = isTRUE(summaryDf$llmConstant[[1L]])

  detMin = summaryDf$detMin[[1L]]
  detMax = summaryDf$detMax[[1L]]
  llmMin = summaryDf$llmMin[[1L]]
  llmMax = summaryDf$llmMax[[1L]]
  meanLlmMinusDet = summaryDf$meanLlmMinusDet[[1L]]

  if (disagreementRate == 0) {
    return("noSystematicDisagreement")
  }

  if (disagreementRate >= 0.8 &&
      directionConsistency >= 0.9 &&
      detConstant &&
      llmConstant &&
      detMax < llmMin) {
    return("deterministicRuleLikelyTooStrict")
  }

  if (disagreementRate >= 0.6 &&
      directionConsistency >= 0.8 &&
      detConstant &&
      !llmConstant &&
      meanLlmMinusDet > 0) {
    return("deterministicRuleLikelyMissingNuance")
  }

  if (disagreementRate >= 0.6 &&
      directionConsistency >= 0.8 &&
      llmConstant &&
      !detConstant &&
      meanLlmMinusDet > 0) {
    return("llmMayBeOverInterpreting")
  }

  if (disagreementRate >= 0.5 && directionConsistency < 0.8) {
    return("rubricOrMetricMayBeAmbiguous")
  }

  if (meanLlmMinusDet > 0) {
    return("llmGenerallyHigherThanDeterministic")
  }

  if (meanLlmMinusDet < 0) {
    return("deterministicGenerallyHigherThanLlm")
  }

  "unclassified"
}
