#' Print a bad explanation grading audit
#'
#' @param x A `wmfmBadExplanationAudit` object.
#' @param digits Number of digits for numeric output.
#' @param maxExamples Maximum number of flagged examples to print in detail.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmBadExplanationAudit = function(
    x,
    digits = 2,
    maxExamples = 10,
    ...
) {

  if (!inherits(x, "wmfmBadExplanationAudit")) {
    stop("`x` must inherit from `wmfmBadExplanationAudit`.", call. = FALSE)
  }

  fmt = function(value) {
    value = suppressWarnings(as.numeric(value)[1])

    if (is.na(value)) {
      return("NA")
    }

    format(round(value, digits), nsmall = digits, trim = TRUE)
  }

  cat("WMFM bad explanation audit\n")
  cat("-------------------------\n")
  cat("Method:", x$method, "\n")
  cat("Number of explanations:", x$nExplanations, "\n")

  if (!is.na(x$goodMark)) {
    cat("Reference good mark:", fmt(x$goodMark), "\n")
  }

  cat("Flag high mark threshold:", fmt(x$flaggedThreshold), "\n")
  cat("Minimum expected drop:", fmt(x$minExpectedDrop), "\n")
  cat("Flagged explanations:", nrow(x$flagged), "\n")

  if (nrow(x$flagged) < 1) {
    cat("\nNo obvious bad explanations were flagged by this audit.\n")
    return(invisible(x))
  }

  cat("\nFlagged explanations\n")

  flaggedNames = utils::head(x$flagged$explanationName, maxExamples)

  for (name in flaggedNames) {
    row = x$summary[x$summary$explanationName == name, , drop = FALSE]
    detail = x$details[[name]]

    cat("- ", name, "\n", sep = "")
    cat("  mark:", fmt(row$mark), "\n")

    if (!is.na(row$goodMark)) {
      cat("  drop vs good:", fmt(row$markDrop), "\n")
    }

    reasons = character(0)

    if (isTRUE(row$flaggedHighMark)) {
      reasons = c(reasons, paste0("mark >= ", fmt(x$flaggedThreshold)))
    }

    if (isTRUE(row$flaggedLowDrop)) {
      reasons = c(reasons, paste0("drop < ", fmt(x$minExpectedDrop)))
    }

    if (row$missedExpectedMetricCount > 0) {
      reasons = c(
        reasons,
        paste0(
          "missed expected metrics: ",
          paste(detail$expectedMissed, collapse = ", ")
        )
      )
    }

    if (length(reasons) > 0) {
      cat("  why flagged:", paste(reasons, collapse = " | "), "\n")
    }

    if (length(detail$expectedDetected) > 0) {
      cat(
        "  expected metrics detected:",
        paste(detail$expectedDetected, collapse = ", "),
        "\n"
      )
    }

    lossDf = detail$metricLossDetails

    if (is.data.frame(lossDf) && nrow(lossDf) > 0) {
      topLosses = utils::head(lossDf[order(-lossDf$marksLost), , drop = FALSE], 4)
      lossLines = vapply(seq_len(nrow(topLosses)), function(i) {
        paste0(
          topLosses$label[i],
          " (-",
          fmt(topLosses$marksLost[i]),
          ")"
        )
      }, character(1))

      cat("  marks lost:", paste(lossLines, collapse = "; "), "\n")
    } else {
      cat("  marks lost: none recorded by the current rubric\n")
    }
  }

  if (nrow(x$flagged) > maxExamples) {
    cat("\n... and", nrow(x$flagged) - maxExamples, "more flagged explanation(s).\n")
  }

  invisible(x)
}
