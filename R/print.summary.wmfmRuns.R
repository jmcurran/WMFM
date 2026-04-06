#' Print a summary.wmfmRuns object
#'
#' @param x A `summary.wmfmRuns` object.
#' @param ... Reserved for future extensions.
#'
#' @return `x`, invisibly.
#' @export
print.summary.wmfmRuns = function(x, ...) {
  if (!inherits(x, "summary.wmfmRuns")) {
    stop("`x` must inherit from `summary.wmfmRuns`.", call. = FALSE)
  }

  cat("WMFM runs summary\n")
  cat("-----------------\n")
  cat("Runs:", x$nRuns, "\n")
  cat("Unique explanations:", x$nUniqueExplanations, "\n")
  cat("Duplicate explanations:", x$duplicateCount, "\n")

  if (!is.na(x$duplicateRate)) {
    cat("Duplicate rate:", round(x$duplicateRate, 3), "\n")
  } else {
    cat("Duplicate rate: NA\n")
  }

  cat("\n")
  cat("Word count\n")
  cat("  Mean:", round(x$wordCount$mean, 2), "\n")
  cat("  Min: ", round(x$wordCount$min, 2), "\n")
  cat("  Max: ", round(x$wordCount$max, 2), "\n")

  cat("\n")
  cat("Sentence count\n")
  cat("  Mean:", round(x$sentenceCount$mean, 2), "\n")
  cat("  Min: ", round(x$sentenceCount$min, 2), "\n")
  cat("  Max: ", round(x$sentenceCount$max, 2), "\n")

  if (!all(is.na(c(
    x$runElapsedSeconds$mean,
    x$runElapsedSeconds$min,
    x$runElapsedSeconds$max
  )))) {
    cat("\n")
    cat("Run elapsed seconds\n")
    cat("  Mean:", round(x$runElapsedSeconds$mean, 2), "\n")
    cat("  Min: ", round(x$runElapsedSeconds$min, 2), "\n")
    cat("  Max: ", round(x$runElapsedSeconds$max, 2), "\n")
  }

  cat("\n")
  cat("Claim frequencies\n")
  print(x$claimSummary, row.names = FALSE)

  invisible(x)
}
