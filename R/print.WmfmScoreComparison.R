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
    cat("  SD difference         :", fmt(x$overallSummary$sdDifferenceRightMinusLeft), "\n\n")
  }

  metricAgreement = x$metricAgreement

  if (!is.null(metricAgreement) && nrow(metricAgreement) > 0) {
    cat("Top disagreements (lowest agreement first):\n")

    ord = order(metricAgreement$proportionEqual, na.last = TRUE)
    showN = min(5, length(ord))

    for (i in seq_len(showN)) {
      idx = ord[i]
      cat(
        "  -", metricAgreement$metric[idx],
        ": agreement =", fmt(metricAgreement$proportionEqual[idx]),
        ", mean diff =", fmt(metricAgreement$meanDifference[idx]),
        "\n"
      )
    }

    cat("\n")
  }

  invisible(x)
}
