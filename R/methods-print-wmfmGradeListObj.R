#' Print a WMFM grade list object
#'
#' @param x A `wmfmGradeListObj` object.
#' @param digits Number of digits for numeric output.
#' @param maxExamples Maximum number of explanation marks to print.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmGradeListObj = function(
    x,
    digits = 2,
    maxExamples = 6,
    ...
) {

  if (!inherits(x, "wmfmGradeListObj")) {
    stop("`x` must inherit from `wmfmGradeListObj`.", call. = FALSE)
  }

  fmt = function(value) {
    value = suppressWarnings(as.numeric(value)[1])

    if (is.na(value)) {
      return("NA")
    }

    format(round(value, digits), nsmall = digits, trim = TRUE)
  }

  cat("WMFM grade list\n")
  cat("---------------\n")
  cat("Number of explanations:", x$meta$nExplanations, "\n")

  if (!is.na(x$meta$method)) {
    cat("Method:", x$meta$method, "\n")
  }

  if (!is.na(x$meta$nLlm) && x$meta$method %in% c("llm", "both")) {
    cat("LLM runs per explanation:", x$meta$nLlm, "\n")
  }

  if (!is.na(x$meta$totalLlmCalls) && x$meta$totalLlmCalls > 0) {
    cat("Total LLM calls:", x$meta$totalLlmCalls, "\n")
  }

  if (!is.na(x$meta$elapsedSeconds)) {
    cat("Elapsed time:", formatWmfmElapsedTime(x$meta$elapsedSeconds), "\n")
  }

  markLines = vapply(utils::head(names(x$grades), maxExamples), function(name) {
    gradeObj = x$grades[[name]]
    paste0(name, ": ", fmt(gradeObj$scores$mark), " / ", gradeObj$scoreScale)
  }, character(1))

  if (length(markLines) > 0) {
    cat("\nMarks\n")
    cat(paste0("  ", markLines, collapse = "\n"), "\n", sep = "")
  }

  if (length(x$grades) > maxExamples) {
    cat("\n... and", length(x$grades) - maxExamples, "more explanation(s).\n")
  }

  invisible(x)
}
