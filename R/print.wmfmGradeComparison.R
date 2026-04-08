#' Print a wmfmGradeComparison object
#'
#' @param x A `wmfmGradeComparison` object.
#' @param digits Number of digits for numeric output.
#' @param maxRows Maximum number of rows to print in each section.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmGradeComparison = function(
    x,
    digits = 2,
    maxRows = 6,
    ...
) {

  if (!inherits(x, "wmfmGradeComparison")) {
    stop("`x` must inherit from `wmfmGradeComparison`.", call. = FALSE)
  }

  fmt = function(v) {
    if (length(v) != 1 || is.na(v)) {
      return("NA")
    }
    format(round(v, digits), nsmall = digits, trim = TRUE)
  }

  cat("WMFM grade comparison\n")
  cat("---------------------\n")
  cat(
    "Compared methods:",
    x$summary$leftMethod,
    "vs",
    x$summary$rightMethod,
    "\n"
  )

  cat("\nOverall\n")
  cat("*", x$summary$leftMethod, "mark:", fmt(x$summary$leftMark), "\n")
  cat("*", x$summary$rightMethod, "mark:", fmt(x$summary$rightMark), "\n")
  cat("* Difference:", fmt(x$summary$markDifference), "\n")

  metricComparison = x$metricComparison
  if (is.data.frame(metricComparison) && nrow(metricComparison) > 0) {
    metricComparison = metricComparison[order(-metricComparison$absDifference, metricComparison$label), , drop = FALSE]
    metricComparison = utils::head(metricComparison, maxRows)

    cat("\nDimension differences\n")
    lines = vapply(
      seq_len(nrow(metricComparison)),
      function(i) {
        paste0(
          "* ",
          metricComparison$label[i],
          ": ",
          metricComparison$leftMethod[i],
          " ",
          fmt(metricComparison$leftValue[i]),
          ", ",
          metricComparison$rightMethod[i],
          " ",
          fmt(metricComparison$rightValue[i]),
          ", difference = ",
          fmt(metricComparison$difference[i])
        )
      },
      character(1)
    )
    cat(paste(lines, collapse = "\n"), "\n")
  }

  rightAdvisory = x$advisoryComparison$right
  if (is.data.frame(rightAdvisory) && nrow(rightAdvisory) > 0) {
    rightAdvisory = utils::head(rightAdvisory, maxRows)

    cat("\n", x$summary$rightMethod, " advisory flags\n", sep = "")
    lines = vapply(
      seq_len(nrow(rightAdvisory)),
      function(i) {
        paste0(
          "* ",
          rightAdvisory$label[i],
          ": ",
          rightAdvisory$detail[i]
        )
      },
      character(1)
    )
    cat(paste(lines, collapse = "\n"), "\n")
  }

  invisible(x)
}
