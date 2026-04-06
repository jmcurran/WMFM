#' Print a scores diagnosis object
#'
#' @param x A \code{wmfmScoresDiagnosis} object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#'
#' @export
print.wmfmScoresDiagnosis = function(x, ...) {
  cat("Scores diagnosis\n")
  cat("----------------\n")
  cat("Metrics diagnosed: ", nrow(x$summaryTable), "\n", sep = "")

  if (nrow(x$flaggedMetrics) > 0L) {
    cat("\nTop flagged metrics:\n")
    print(
      utils::head(
        x$flaggedMetrics[, intersect(
          c(
            "metric",
            "diagnosisClass",
            "likelyIssue",
            "disagreementRate",
            "meanLlmMinusDet",
            "priorityScore"
          ),
          names(x$flaggedMetrics)
        ), drop = FALSE],
        10
      )
    )
  } else {
    cat("\nNo diagnosable metrics found.\n")
  }

  invisible(x)
}
