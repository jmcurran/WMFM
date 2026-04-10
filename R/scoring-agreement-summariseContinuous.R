#' Summarise agreement for a continuous metric
#'
#' Summarises agreement between two continuous vectors using mean
#' differences, absolute differences, correlation, and Bland-Altman
#' limits of agreement.
#'
#' @param leftVec Vector of left-side values.
#' @param rightVec Vector of right-side values.
#' @param metricRow Single-row metric registry data frame.
#'
#' @return A one-row data frame, or `NULL` if no complete pairs are available.
#' @keywords internal
summariseContinuousAgreement = function(leftVec, rightVec, metricRow) {
  ok = !(is.na(leftVec) | is.na(rightVec))

  if (sum(ok) == 0) {
    return(NULL)
  }

  left = as.numeric(leftVec[ok])
  right = as.numeric(rightVec[ok])
  diff = right - left
  sdDiff = if (length(diff) > 1) stats::sd(diff) else NA_real_
  corVal = if (length(left) > 1) stats::cor(left, right) else NA_real_

  data.frame(
    metric = metricRow$metricName,
    label = metricRow$label,
    group = metricRow$group,
    metricType = metricRow$metricType,
    nCompared = length(diff),
    meanDifference = mean(diff),
    meanAbsoluteDifference = mean(abs(diff)),
    correlation = corVal,
    sdDifference = sdDiff,
    loaLower = mean(diff) - 1.96 * sdDiff,
    loaUpper = mean(diff) + 1.96 * sdDiff,
    stringsAsFactors = FALSE
  )
}
