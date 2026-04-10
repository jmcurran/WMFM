#' Print a WMFM score stability object
#'
#' Provides a concise console summary of within-method score stability.
#'
#' @param x A `wmfmScoreStability` object.
#' @param digits Integer. Number of digits to display.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmScoreStability = function(
    x,
    digits = 2,
    ...
) {

  if (!inherits(x, "wmfmScoreStability")) {
    stop("Object is not of class `wmfmScoreStability`.", call. = FALSE)
  }

  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }

  fmt = function(v) {
    ifelse(
      is.na(v),
      "NA",
      formatC(v, digits = digits, format = "f")
    )
  }

  cat("\nWMFM Score Stability\n")
  cat("--------------------\n")
  cat("Methods assessed :", paste(x$methods, collapse = ", "), "\n")
  cat("Runs per method  :", x$nRuns, "\n\n")

  if (!is.null(x$overallSummary$overallScore) && nrow(x$overallSummary$overallScore) > 0) {
    cat("Overall score variability by method:\n")

    overallDf = x$overallSummary$overallScore

    for (i in seq_len(nrow(overallDf))) {
      cat(
        "  -", overallDf$method[i],
        ": mean =", fmt(overallDf$mean[i]),
        ", sd =", fmt(overallDf$sd[i]),
        ", range =", fmt(overallDf$range[i]),
        "\n"
      )
    }

    cat("\n")
  }

  if (!is.null(x$ordinalStability) && nrow(x$ordinalStability) > 0) {
    cat("Least stable ordinal metrics:\n")

    ord = order(
      x$ordinalStability$modalProportion,
      -x$ordinalStability$range,
      na.last = TRUE
    )
    showN = min(5, length(ord))

    for (i in seq_len(showN)) {
      idx = ord[i]
      cat(
        "  -", x$ordinalStability$method[idx], "/", x$ordinalStability$metric[idx],
        ": modal proportion =", fmt(x$ordinalStability$modalProportion[idx]),
        ", range =", fmt(x$ordinalStability$range[idx]),
        "\n"
      )
    }

    cat("\n")
  }

  if (!is.null(x$binaryStability) && nrow(x$binaryStability) > 0) {
    cat("Binary metrics that vary across runs:\n")

    varying = x$binaryStability[x$binaryStability$variesAcrossRuns, , drop = FALSE]

    if (nrow(varying) == 0) {
      cat("  None\n\n")
    } else {
      showN = min(5, nrow(varying))

      for (i in seq_len(showN)) {
        cat(
          "  -", varying$method[i], "/", varying$metric[i],
          ": true rate =", fmt(varying$trueRate[i]),
          ", modal proportion =", fmt(varying$modalProportion[i]),
          "\n"
        )
      }

      cat("\n")
    }
  }

  invisible(x)
}
