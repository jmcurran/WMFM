#' Print a WMFM score comparison object
#'
#' Provides a concise console summary of agreement between two WMFM scoring
#' result sets.
#'
#' @param x A `wmfmScoreComparison` object.
#' @param digits Integer. Number of digits to display.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmScoreComparison = function(
    x,
    digits = 2,
    ...
) {

  if (!inherits(x, "wmfmScoreComparison")) {
    stop("Object is not of class `wmfmScoreComparison`.", call. = FALSE)
  }

  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }

  fmt = function(v) {
    ifelse(
      is.na(v),
      "NA",
      formatC(v, digits = digits, format = "f")
    )
  }

  cat("\nWMFM Score Comparison\n")
  cat("---------------------\n")
  cat("Source          :", x$source, "\n")
  cat("Left method     :", x$leftMethod, "\n")
  cat("Right method    :", x$rightMethod, "\n")
  cat("Runs compared   :", x$nRunsCompared, "\n\n")

  if (!is.null(x$overallSummary)) {
    cat("Overall score summary:\n")
    cat("  Left mean             :", fmt(x$overallSummary$meanLeftOverallScore), "\n")
    cat("  Right mean            :", fmt(x$overallSummary$meanRightOverallScore), "\n")
    cat("  Mean difference       :", fmt(x$overallSummary$meanDifferenceRightMinusLeft), "\n")
    cat("  Mean abs difference   :", fmt(x$overallSummary$meanAbsoluteDifference), "\n")
    cat("  SD difference         :", fmt(x$overallSummary$sdDifferenceRightMinusLeft), "\n\n")
  }

  if (!is.null(x$ordinalAgreement) && nrow(x$ordinalAgreement) > 0) {
    cat("Ordinal metrics with lowest weighted agreement:\n")
    ord = order(x$ordinalAgreement$weightedKappa, na.last = TRUE)
    showN = min(5, length(ord))

    for (i in seq_len(showN)) {
      idx = ord[i]
      cat(
        "  -", x$ordinalAgreement$metric[idx],
        ": kappa =", fmt(x$ordinalAgreement$weightedKappa[idx]),
        ", exact =", fmt(x$ordinalAgreement$proportionEqual[idx]),
        ", adjacent =", fmt(x$ordinalAgreement$proportionAdjacent[idx]),
        ", MAD =", fmt(x$ordinalAgreement$meanAbsoluteDifference[idx]),
        "\n"
      )
    }

    cat("\n")
  }

  if (!is.null(x$continuousAgreement) && nrow(x$continuousAgreement) > 0) {
    cat("Continuous metrics summary:\n")
    ord = order(x$continuousAgreement$meanAbsoluteDifference, decreasing = TRUE, na.last = TRUE)
    showN = min(5, length(ord))

    for (i in seq_len(showN)) {
      idx = ord[i]
      cat(
        "  -", x$continuousAgreement$metric[idx],
        ": mean diff =", fmt(x$continuousAgreement$meanDifference[idx]),
        ", MAD =", fmt(x$continuousAgreement$meanAbsoluteDifference[idx]),
        ", cor =", fmt(x$continuousAgreement$correlation[idx]),
        "\n"
      )
    }

    cat("\n")
  }

  invisible(x)
}
