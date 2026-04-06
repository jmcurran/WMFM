#' Print a metric diagnosis object
#'
#' @param x A \code{wmfmMetricDiagnosis} object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#'
#' @export
print.wmfmMetricDiagnosis = function(x, ...) {
  cat("Metric diagnosis\n")
  cat("---------------\n")
  cat("Metric: ", x$metric, "\n", sep = "")

  s = x$summary[1, , drop = FALSE]

  cat("Diagnosis class: ", s$diagnosisClass, "\n", sep = "")
  cat("Runs: ", s$nRuns, "\n", sep = "")
  cat("Disagreement rate: ", round(s$disagreementRate, 3), "\n", sep = "")
  cat("Mean LLM - deterministic: ", round(s$meanLlmMinusDet, 3), "\n", sep = "")
  cat("Direction consistency: ", round(s$directionConsistency, 3), "\n", sep = "")
  cat("Deterministic constant: ", s$detConstant, "\n", sep = "")
  cat("LLM constant: ", s$llmConstant, "\n", sep = "")

  if (!is.null(x$evidenceSummary) && is.data.frame(x$evidenceSummary) && nrow(x$evidenceSummary) > 0L) {
    e = x$evidenceSummary[1, , drop = FALSE]

    cat("\nEvidence summary:\n")

    if ("likelyIssue" %in% names(e)) {
      cat("- likely issue: ", e$likelyIssue, "\n", sep = "")
    }

    if ("effectScale_notStated_n" %in% names(e) && !is.na(e$effectScale_notStated_n)) {
      cat("- effectScaleClaim = not_stated: ", e$effectScale_notStated_n, "\n", sep = "")
    }

    if ("effectScale_mixedOrUnclear_n" %in% names(e) && !is.na(e$effectScale_mixedOrUnclear_n)) {
      cat("- effectScaleClaim = mixed_or_unclear: ", e$effectScale_mixedOrUnclear_n, "\n", sep = "")
    }

    if ("effectScale_additive_n" %in% names(e) && !is.na(e$effectScale_additive_n)) {
      cat("- effectScaleClaim = additive: ", e$effectScale_additive_n, "\n", sep = "")
    }

    if ("percentLanguageMention_rate" %in% names(e) && !is.na(e$percentLanguageMention_rate)) {
      cat("- percentLanguageMention rate: ", round(e$percentLanguageMention_rate, 3), "\n", sep = "")
    }

    if ("comparisonLanguageMention_rate" %in% names(e) && !is.na(e$comparisonLanguageMention_rate)) {
      cat("- comparisonLanguageMention rate: ", round(e$comparisonLanguageMention_rate, 3), "\n", sep = "")
    }

    if ("conditionalLanguageMention_rate" %in% names(e) && !is.na(e$conditionalLanguageMention_rate)) {
      cat("- conditionalLanguageMention rate: ", round(e$conditionalLanguageMention_rate, 3), "\n", sep = "")
    }
  }

  if (nrow(x$flaggedRuns) > 0L) {
    cat("\nFlagged runs:\n")
    print(
      x$flaggedRuns[, intersect(
        c(
          "runId",
          "detValue",
          "llmValue",
          "llmMinusDet",
          "disagreementDirection"
        ),
        names(x$flaggedRuns)
      ), drop = FALSE]
    )
  } else {
    cat("\nNo flagged disagreement runs.\n")
  }

  invisible(x)
}
