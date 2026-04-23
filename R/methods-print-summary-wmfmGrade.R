#' Print a WMFM grade summary
#'
#' @param x A `summary.wmfmGrade` object.
#' @param digits Number of digits for numeric output.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.summary.wmfmGrade = function(
    x,
    digits = 2,
    ...
) {

  if (!inherits(x, "summary.wmfmGrade")) {
    stop("`x` must inherit from `summary.wmfmGrade`.", call. = FALSE)
  }

  fmt = function(value) {
    value = suppressWarnings(as.numeric(value)[1])

    if (is.na(value)) {
      return("NA")
    }

    format(round(value, digits), nsmall = digits, trim = TRUE)
  }

  cat("WMFM grade summary\n")
  cat("------------------\n")
  cat("Method:", x$method, "\n")
  cat("Runs:", x$nRuns, "\n")
  cat(
    "Mark mean:", fmt(x$mark$mean),
    "[", fmt(x$mark$min), ", ", fmt(x$mark$max), "]\n",
    sep = ""
  )
  cat(
    "Overall score mean:", fmt(x$overallScore$mean),
    "[", fmt(x$overallScore$min), ", ", fmt(x$overallScore$max), "]\n",
    sep = ""
  )

  if (!is.null(x$dimensionSummary) && is.data.frame(x$dimensionSummary) && nrow(x$dimensionSummary) > 0) {
    cat("\nDimension summary\n")
    for (i in seq_len(nrow(x$dimensionSummary))) {
      row = x$dimensionSummary[i, , drop = FALSE]
      cat(
        "  ", row$label, ": ",
        fmt(row$mean),
        " [", fmt(row$min), ", ", fmt(row$max), "] / ",
        fmt(row$maxValue),
        "\n",
        sep = ""
      )
    }
  }

  if (!is.na(x$elapsedSeconds)) {
    cat("\nElapsed time:", formatWmfmElapsedTime(x$elapsedSeconds), "\n")
  }

  if (!is.na(x$meanSecondsPerRun)) {
    cat("Mean time per run:", formatWmfmElapsedTime(x$meanSecondsPerRun), "\n")
  }

  invisible(x)
}
