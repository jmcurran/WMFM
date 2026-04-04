#' Plot a WMFM score comparison object
#'
#' @param x A `wmfmScoreComparison` object.
#' @param type "agreement" or "overall"
#' @export
plot.wmfmScoreComparison = function(x, type = c("agreement", "overall"), ...) {

  type = match.arg(type)

  if (type == "overall") {

    df = x$pairedOverallScores

    meanDiff = x$overallSummary$meanDifferenceRightMinusLeft
    lowerLoA = x$overallSummary$loaLower
    upperLoA = x$overallSummary$loaUpper

    return(
      ggplot2::ggplot(df, ggplot2::aes(meanOverallScore, differenceOverallScore)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = meanDiff) +
        ggplot2::geom_hline(yintercept = lowerLoA, linetype = 2) +
        ggplot2::geom_hline(yintercept = upperLoA, linetype = 2) +
        ggplot2::theme_bw()
    )
  }

  df = x$ordinalAgreement

  ggplot2::ggplot(df, ggplot2::aes(metric, weightedKappa)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()
}
