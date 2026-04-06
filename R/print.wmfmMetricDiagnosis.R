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
