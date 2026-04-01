
#' Print a WMFM score comparison object
#'
#' Provides a concise console summary of agreement between deterministic and
#' LLM scoring methods.
#'
#' @param x Object produced by `compareScores()`.
#' @param digits Integer. Number of digits to display for numeric summaries.
#'   Defaults to `2`.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return Invisibly returns `x`.
#' @export
print.WmfmScoreComparison = function(
    x,
    digits = 2,
    ...
) {

  if (!inherits(x, "WmfmScoreComparison")) {
    stop("Object is not of class 'WmfmScoreComparison'.", call. = FALSE)
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
  cat("----------------------\n")
  cat("Runs compared     :", x$nRunsCompared, "\n")
  cat("Deterministic set :", x$deterministicPrimaryMethod, "\n")
  cat("LLM set           :", x$llmPrimaryMethod, "\n\n")

  if (!is.null(x$overallSummary)) {
    cat("Overall score summary:\n")
    cat("  Deterministic mean :", fmt(x$overallSummary$meanDeterministicOverallScore), "\n")
    cat("  LLM mean           :", fmt(x$overallSummary$meanLlmOverallScore), "\n")
    cat("  Mean difference    :", fmt(x$overallSummary$meanDifferenceLlmMinusDeterministic), "\n")
    cat("  SD difference      :", fmt(x$overallSummary$sdDifferenceLlmMinusDeterministic), "\n\n")
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
