#' Print a WMFM grade list summary
#'
#' @param x A `summary.wmfmGradeListObj` object.
#' @param digits Number of digits for numeric output.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.summary.wmfmGradeListObj = function(
    x,
    digits = 2,
    ...
) {

  if (!inherits(x, "summary.wmfmGradeListObj")) {
    stop("`x` must inherit from `summary.wmfmGradeListObj`.", call. = FALSE)
  }

  fmt = function(value) {
    value = suppressWarnings(as.numeric(value)[1])

    if (is.na(value)) {
      return("NA")
    }

    format(round(value, digits), nsmall = digits, trim = TRUE)
  }

  cat("WMFM grade list summary\n")
  cat("-----------------------\n")
  cat("Number of explanations:", x$nExplanations, "\n")

  if (!is.na(x$method)) {
    cat("Method:", x$method, "\n")
  }

  if (!is.na(x$nLlm) && x$nLlm > 0) {
    cat("LLM runs per explanation:", x$nLlm, "\n")
  }

  if (!is.na(x$totalLlmCalls) && x$totalLlmCalls > 0) {
    cat("Total LLM calls:", x$totalLlmCalls, "\n")
  }

  if (!is.null(x$deterministicMark)) {
    cat(
      "\nDeterministic mark mean: ", fmt(x$deterministicMark$mean),
      " [", fmt(x$deterministicMark$min), ", ", fmt(x$deterministicMark$max), "]\n",
      sep = ""
    )
  }

  if (!is.null(x$llmMark)) {
    cat(
      "LLM mark mean: ", fmt(x$llmMark$mean),
      " [", fmt(x$llmMark$min), ", ", fmt(x$llmMark$max), "]\n",
      sep = ""
    )
  }

  if (!is.na(x$meanDifference)) {
    cat("Mean difference (LLM - deterministic):", fmt(x$meanDifference), "\n")
  }

  if (!is.na(x$elapsedSeconds)) {
    cat("Elapsed time:", formatWmfmElapsedTime(x$elapsedSeconds), "\n")
  }

  invisible(x)
}
