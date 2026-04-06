#' Compute a priority score for metric diagnosis
#'
#' Internal helper that ranks metrics for follow-up. Higher values indicate
#' more systematic and potentially more important disagreement.
#'
#' @param summaryTable A data frame of per-metric diagnosis summaries.
#'
#' @return A numeric vector of priority scores.
#'
#' @keywords internal
computeDiagnosisPriorityScore = function(summaryTable) {
  if (!is.data.frame(summaryTable) || nrow(summaryTable) < 1L) {
    return(numeric(0))
  }

  disagreementTerm = 100 * summaryTable$disagreementRate
  biasTerm = 20 * abs(summaryTable$meanLlmMinusDet)
  consistencyTerm = 20 * summaryTable$directionConsistency
  constantGapBonus = ifelse(
    isTRUE(summaryTable$detConstant) & isTRUE(summaryTable$llmConstant),
    15,
    0
  )

  disagreementTerm + biasTerm + consistencyTerm + constantGapBonus
}
