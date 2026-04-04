#' Summarize agreement for an ordinal metric
#'
#' Summarizes agreement between two ordinal vectors using exact
#' agreement, adjacent agreement, mean differences, and quadratic
#' weighted kappa.
#'
#' @param leftVec Vector of left-side values.
#' @param rightVec Vector of right-side values.
#' @param metricRow Single-row metric registry data frame.
#'
#' @return A one-row data frame, or `NULL` if no complete pairs are available.
#' @keywords internal
summarizeOrdinalAgreement = function(leftVec, rightVec, metricRow) {
  ok = !(is.na(leftVec) | is.na(rightVec))

  if (sum(ok) == 0) {
    return(NULL)
  }

  orderedLevels = metricRow$orderedLevels[[1]]
  left = match(as.character(leftVec[ok]), orderedLevels)
  right = match(as.character(rightVec[ok]), orderedLevels)
  ok2 = !(is.na(left) | is.na(right))

  if (sum(ok2) == 0) {
    return(NULL)
  }

  left = left[ok2]
  right = right[ok2]
  diff = right - left

  data.frame(
    metric = metricRow$metricName,
    label = metricRow$label,
    group = metricRow$group,
    metricType = metricRow$metricType,
    nCompared = length(diff),
    proportionEqual = mean(diff == 0),
    proportionAdjacent = mean(abs(diff) <= 1),
    meanDifference = mean(diff),
    meanAbsoluteDifference = mean(abs(diff)),
    weightedKappa = computeWeightedKappa(left, right),
    stringsAsFactors = FALSE
  )
}
