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

  chooseDisplayMethod = function(gradeObj) {
    method = gradeObj$meta$lastScoredMethod %||% NA_character_

    if (length(method) == 1 && !is.na(method) && nzchar(method) &&
        method %in% names(gradeObj$scores$byMethod %||% list())) {
      return(method)
    }

    methods = names(gradeObj$scores$byMethod %||% list())

    if (length(methods) < 1) {
      return(NA_character_)
    }

    methods[length(methods)]
  }

  extractMark = function(gradeObj, method) {
    if (is.na(method) || !method %in% names(gradeObj$scores$byMethod %||% list())) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(gradeObj$scores$byMethod[[method]]$mark)[1])
  }

  cat("WMFM grade list\n")
  cat("---------------\n")
  cat("Number of explanations:", x$meta$nExplanations, "\n")

  if (!is.na(x$meta$method)) {
    cat("Requested method:", x$meta$method, "\n")
  }

  if (!is.null(x$meta$lastScoredMethod) && !is.na(x$meta$lastScoredMethod)) {
    cat("Last scored method:", x$meta$lastScoredMethod, "\n")
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

  gradeNames = utils::head(names(x$grades), maxExamples)
  markLines = vapply(gradeNames, function(name) {
    gradeObj = x$grades[[name]]
    method = chooseDisplayMethod(gradeObj)
    mark = extractMark(gradeObj, method)
    methodLabel = if (is.na(method)) "unscored" else method

    paste0(
      name, ": ",
      fmt(mark), " / ", gradeObj$scoreScale,
      " (", methodLabel, ")"
    )
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
