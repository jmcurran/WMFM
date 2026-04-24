#' Order WMFM agreement metrics for plotting
#'
#' Computes an ordering for ordinal agreement metrics so that the least
#' agreeable metrics can be shown first in plots. The default ordering combines
#' weighted kappa, exact agreement, adjacent agreement, and mean absolute
#' difference into a simple composite disagreement score.
#'
#' @param agreementDf A data frame like `x$ordinalAgreement` from a
#'   `wmfmScoreComparison` object.
#' @param orderBy Character. One of `"worst"` or `"registry"`.
#'
#' @return A character vector of metric labels in plotting order.
#' @keywords internal
orderWmfmAgreementMetrics = function(
    agreementDf,
    orderBy = c("worst", "registry")
) {

  orderBy = match.arg(orderBy)

  if (!is.data.frame(agreementDf) || nrow(agreementDf) == 0) {
    return(character(0))
  }

  if (identical(orderBy, "registry")) {
    return(as.character(agreementDf$label))
  }

  kappaForOrder = agreementDf$weightedKappa
  kappaForOrder[is.na(kappaForOrder)] = -1

  madScaled = agreementDf$meanAbsoluteDifference / 2
  madScaled[is.na(madScaled)] = 1

  disagreementScore =
    (1 - kappaForOrder) +
    (1 - agreementDf$proportionAdjacent) +
    (1 - agreementDf$proportionEqual) +
    madScaled

  as.character(agreementDf$label[order(disagreementScore, decreasing = TRUE)])
}


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


#' Summarise agreement for an ordinal metric
#'
#' summarises agreement between two ordinal vectors using exact
#' agreement, adjacent agreement, mean differences, and quadratic
#' weighted kappa.
#'
#' WMFM ordinal judged fields are currently stored as integer-like values
#' \code{0}, \code{1}, and \code{2}. The registry's \code{orderedLevels}
#' is used as the source of truth for mapping values onto the ordinal scale.
#'
#' @param leftVec Vector of left-side values.
#' @param rightVec Vector of right-side values.
#' @param metricRow Single-row metric registry data frame.
#'
#' @return A one-row data frame, or \code{NULL} if no complete pairs are available.
#' @keywords internal
summariseOrdinalAgreement = function(leftVec, rightVec, metricRow) {
  ok = !(is.na(leftVec) | is.na(rightVec))

  if (sum(ok) == 0) {
    return(NULL)
  }

  orderedLevels = metricRow$orderedLevels[[1]]

  if (is.null(orderedLevels) || length(orderedLevels) < 2) {
    stop(
      "Ordinal metric `", metricRow$metricName, "` must define orderedLevels.",
      call. = FALSE
    )
  }

  leftRaw = leftVec[ok]
  rightRaw = rightVec[ok]

  left = match(as.character(leftRaw), as.character(orderedLevels))
  right = match(as.character(rightRaw), as.character(orderedLevels))
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


#' Compute quadratic weighted kappa for ordinal values
#'
#' Computes quadratic weighted kappa for two ordinal vectors that have
#' already been converted to integer scores.
#'
#' @param leftVec Integer-like vector of left-side ordinal values.
#' @param rightVec Integer-like vector of right-side ordinal values.
#'
#' @return A numeric scalar, or `NA_real_` if it cannot be computed.
#' @keywords internal
computeWeightedKappa = function(leftVec, rightVec) {
  ok = !(is.na(leftVec) | is.na(rightVec))
  leftVec = as.integer(leftVec[ok])
  rightVec = as.integer(rightVec[ok])

  if (length(leftVec) == 0) {
    return(NA_real_)
  }

  cats = sort(unique(c(leftVec, rightVec)))
  k = length(cats)

  if (k <= 1) {
    return(1)
  }

  li = match(leftVec, cats)
  ri = match(rightVec, cats)

  obs = matrix(0, nrow = k, ncol = k)

  for (i in seq_along(li)) {
    obs[li[i], ri[i]] = obs[li[i], ri[i]] + 1
  }

  obs = obs / sum(obs)
  rowM = rowSums(obs)
  colM = colSums(obs)
  exp = outer(rowM, colM)

  w = outer(
    seq_len(k),
    seq_len(k),
    FUN = function(i, j) {
      (abs(i - j) / (k - 1))^2
    }
  )

  observedWeightedDisagreement = sum(w * obs)
  expectedWeightedDisagreement = sum(w * exp)

  if (expectedWeightedDisagreement == 0) {
    return(NA_real_)
  }

  1 - (observedWeightedDisagreement / expectedWeightedDisagreement)
}
