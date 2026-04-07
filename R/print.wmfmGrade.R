#' Print a WMFM grade object
#'
#' Prints a concise summary of a graded explanation, including the mark, key
#' dimension scores, strengths, weaknesses, missing elements, and the main
#' places where marks were lost.
#'
#' @param x A `wmfmGrade` object.
#' @param digits Integer number of digits to print for numeric values.
#' @param maxRows Integer maximum number of rows to print for each feedback
#'   section.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmGrade = function(
    x,
    digits = 2,
    maxRows = 6,
    ...
) {

  if (!inherits(x, "wmfmGrade")) {
    stop("`x` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  cat("WMFM grade\n")
  cat("----------\n")

  if (!isTRUE(x$meta$scored)) {
    cat("Status: not yet scored\n")
    cat("Call `score()` on this object to produce a mark.\n")
    return(invisible(x))
  }

  overallScore = x$scores$overallScore
  mark = x$scores$mark
  scale = x$scoreScale
  wordCount = x$scores$student$wordCount[1] %||% NA_integer_

  cat("Mark:", format(round(mark, digits = digits), nsmall = digits), "/", scale, "\n")
  cat("Overall score:", format(round(overallScore, digits = digits), nsmall = digits), "/ 100\n")
  cat("Words:", wordCount, "\n")

  dimensionMetrics = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore"
  )

  if (is.data.frame(x$scores$metricSummary)) {
    dims = x$scores$metricSummary[
      x$scores$metricSummary$metric %in% dimensionMetrics,
      c("label", "studentValue", "maxValue"),
      drop = FALSE
    ]

    if (nrow(dims) > 0) {
      cat("\nDimension scores\n")
      print(dims, row.names = FALSE)
    }
  }

  strengths = x$feedback$strengths
  if (is.data.frame(strengths) && nrow(strengths) > 0) {
    cat("\nStrengths\n")
    strengthPrint = utils::head(strengths[, c("label", "comment"), drop = FALSE], maxRows)
    print(strengthPrint, row.names = FALSE)
  }

  weaknesses = x$feedback$weaknesses
  if (is.data.frame(weaknesses) && nrow(weaknesses) > 0) {
    cat("\nWeaknesses\n")
    weaknessPrint = utils::head(weaknesses[, c("label", "marksLost", "reason"), drop = FALSE], maxRows)
    print(weaknessPrint, row.names = FALSE)
  }

  missingElements = x$feedback$missingElements
  if (is.data.frame(missingElements) && nrow(missingElements) > 0) {
    cat("\nMissing or underdeveloped elements\n")
    missingPrint = utils::head(missingElements[, c("label", "marksLost", "detail"), drop = FALSE], maxRows)
    print(missingPrint, row.names = FALSE)
  }

  losses = x$feedback$whereMarksLost
  if (is.data.frame(losses) && nrow(losses) > 0) {
    cat("\nDetailed mark losses\n")
    print(utils::head(losses, maxRows), row.names = FALSE)
  } else {
    cat("\nDetailed mark losses\n")
    cat("None detected by the current rubric.\n")
  }

  comparison = x$feedback$modelAnswerComparison
  if (is.data.frame(comparison) && nrow(comparison) > 0) {
    cat("\nCompared with the supplied model answer\n")
    comparisonPrint = utils::head(
      comparison[, c("label", "referenceDelta", "comment"), drop = FALSE],
      maxRows
    )
    print(comparisonPrint, row.names = FALSE)
  }

  invisible(x)
}
