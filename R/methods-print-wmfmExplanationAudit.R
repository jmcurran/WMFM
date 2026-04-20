#' Print a WMFM explanation audit
#'
#' @param x A `wmfmExplanationAudit` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmExplanationAudit = function(x, ...) {

  cat("WMFM explanation audit\n")
  cat("---------------------\n")
  cat(x$transparencyNote %||% "", "\n\n", sep = "")

  if (!is.null(x$interpretationScale)) {
    cat("Interpretation scale\n")
    cat("- Fitted-value scale: ", x$interpretationScale$fittedValueScale %||% "", "\n", sep = "")
    cat("- Effect scale: ", x$interpretationScale$effectScale %||% "", "\n", sep = "")
    cat("- Back-transformation: ", x$interpretationScale$backTransformation %||% "", "\n\n", sep = "")
  }

  if (is.data.frame(x$numericAnchor$table) && nrow(x$numericAnchor$table) > 0) {
    cat("Numeric anchors\n")
    print(x$numericAnchor$table)
    cat("\n")
  }

  if (is.data.frame(x$referenceLevels) && nrow(x$referenceLevels) > 0) {
    cat("Reference levels\n")
    print(x$referenceLevels)
    cat("\n")
  }

  invisible(x)
}
