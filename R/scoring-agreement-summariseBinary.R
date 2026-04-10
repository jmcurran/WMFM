#' Summarise agreement for a binary metric
#'
#' Summarises agreement between two binary vectors.
#'
#' @param leftVec Vector of left-side values.
#' @param rightVec Vector of right-side values.
#' @param metricRow Single-row metric registry data frame.
#'
#' @return A one-row data frame, or `NULL` if no complete pairs are available.
#' @keywords internal
summariseBinaryAgreement = function(leftVec, rightVec, metricRow) {
  ok = !(is.na(leftVec) | is.na(rightVec))

  if (sum(ok) == 0) {
    return(NULL)
  }

  left = as.logical(leftVec[ok])
  right = as.logical(rightVec[ok])

  data.frame(
    metric = metricRow$metricName,
    label = metricRow$label,
    group = metricRow$group,
    metricType = metricRow$metricType,
    nCompared = length(left),
    proportionEqual = mean(left == right),
    trueRateLeft = mean(left),
    trueRateRight = mean(right),
    stringsAsFactors = FALSE
  )
}
