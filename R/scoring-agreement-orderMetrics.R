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
