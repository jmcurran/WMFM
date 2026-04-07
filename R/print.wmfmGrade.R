#' Print a WMFM grade object
#'
#' Prints a concise summary of a graded explanation, including the mark, key
#' dimension scores, strengths, weaknesses, missing elements, and the main
#' places where marks were lost.
#'
#' @param x A `wmfmGrade` object.
#' @param method Optional character. One of `"deterministic"` or `"llm"`.
#'   When omitted, the most recently scored method is shown if available.
#' @param digits Integer number of digits to print for numeric values.
#' @param maxRows Integer maximum number of rows to print for each feedback
#'   section.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmGrade = function(
    x,
    method = NULL,
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

  availableMethods = names(x$scores$byMethod %||% list())

  if (is.null(method)) {
    method = x$meta$lastScoredMethod %||% NA_character_

    if (!is.character(method) || length(method) != 1 || is.na(method) || !nzchar(method)) {
      if ("deterministic" %in% availableMethods) {
        method = "deterministic"
      } else if ("llm" %in% availableMethods) {
        method = "llm"
      }
    }
  } else {
    method = match.arg(method, choices = c("deterministic", "llm"))
  }

  if (!method %in% availableMethods) {
    stop("No grade is available for method `", method, "`.", call. = FALSE)
  }

  scoreBlock = x$scores$byMethod[[method]]
  feedbackBlock = x$feedback$byMethod[[method]]

  overallScore = scoreBlock$overallScore
  mark = scoreBlock$mark
  scale = x$scoreScale
  wordCount = scoreBlock$student$wordCount[1] %||% NA_integer_

  cat("Method:", method, "\n")
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

  if (is.data.frame(scoreBlock$metricSummary)) {
    dims = scoreBlock$metricSummary[
      scoreBlock$metricSummary$metric %in% dimensionMetrics,
      c("label", "studentValue", "maxValue"),
      drop = FALSE
    ]

    if (nrow(dims) > 0) {
      cat("\nDimension scores\n")
      print(dims, row.names = FALSE)
    }
  }

  strengths = feedbackBlock$strengths
  if (is.data.frame(strengths) && nrow(strengths) > 0) {
    cat("\nStrengths\n")
    strengthPrint = utils::head(strengths[, c("label", "comment"), drop = FALSE], maxRows)
    print(strengthPrint, row.names = FALSE)
  }

  weaknesses = feedbackBlock$weaknesses
  if (is.data.frame(weaknesses) && nrow(weaknesses) > 0) {
    cat("\nWeaknesses\n")
    weaknessPrint = utils::head(weaknesses[, c("label", "marksLost", "reason"), drop = FALSE], maxRows)
    print(weaknessPrint, row.names = FALSE)
  }

  missingElements = feedbackBlock$missingElements
  if (is.data.frame(missingElements) && nrow(missingElements) > 0) {
    cat("\nMissing or underdeveloped elements\n")
    missingPrint = utils::head(missingElements[, c("label", "marksLost", "detail"), drop = FALSE], maxRows)
    print(missingPrint, row.names = FALSE)
  }

  losses = feedbackBlock$whereMarksLost
  if (is.data.frame(losses) && nrow(losses) > 0) {
    cat("\nDetailed mark losses\n")
    print(utils::head(losses, maxRows), row.names = FALSE)
  } else {
    cat("\nDetailed mark losses\n")
    cat("None detected by the current rubric.\n")
  }

  comparison = feedbackBlock$modelAnswerComparison
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
