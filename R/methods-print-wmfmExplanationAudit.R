#' Print a WMFM explanation audit
#'
#' @param x A `wmfmExplanationAudit` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmExplanationAudit = function(x, ...) {

  validateWmfmExplanationAudit(x = x)

  cat("WMFM explanation audit\n")
  cat("---------------------\n")
  cat(x$transparencyNote %||% "", "\n\n", sep = "")

  if (!is.null(x$overview)) {
    cat("Overview\n")
    cat("- Response: ", x$overview$response %||% "", "\n", sep = "")
    cat("- Predictors: ", paste(x$overview$predictors %||% character(0), collapse = ", "), "\n", sep = "")
    cat("- Observations: ", x$overview$nObservations %||% "", "\n", sep = "")
    cat("- Family/link: ", x$overview$modelFamily %||% "", " / ", x$overview$link %||% "", "\n\n", sep = "")
  }

  if (!is.null(x$interpretationScale)) {
    cat("Interpretation scale\n")
    cat("- Fitted-value scale: ", x$interpretationScale$fittedValueScale %||% "", "\n", sep = "")
    cat("- Effect scale: ", x$interpretationScale$effectScale %||% "", "\n", sep = "")
    cat("- Back-transformation: ", x$interpretationScale$backTransformation %||% "", "\n\n", sep = "")
  }

  if (!is.null(x$numericAnchor)) {
    cat("Numeric anchors\n")
    cat("- Reference rule: ", x$numericAnchor$numericReference %||% "", "\n", sep = "")
    cat("- Note: ", x$numericAnchor$note %||% "", "\n", sep = "")

    if (is.data.frame(x$numericAnchor$table) && nrow(x$numericAnchor$table) > 0) {
      print(x$numericAnchor$table)
    } else {
      cat("(none)\n")
    }
    cat("\n")
  }

  cat("Reference levels\n")
  if (is.data.frame(x$referenceLevels) && nrow(x$referenceLevels) > 0) {
    print(x$referenceLevels)
  } else {
    cat("(none)\n")
  }
  cat("\n")

  if (!is.null(x$confidenceIntervals)) {
    cat("Confidence intervals\n")
    cat("- Level: ", x$confidenceIntervals$level %||% "", "\n", sep = "")
    cat("- Mode: ", x$confidenceIntervals$mode %||% "", "\n", sep = "")
    cat("- Displayed scales: ", paste(x$confidenceIntervals$displayedScales %||% character(0), collapse = ", "), "\n\n", sep = "")
  }

  cat("Evidence tables\n")
  cat("- Baseline rows: ", if (is.data.frame(x$baselineEvidence)) nrow(x$baselineEvidence) else 0L, "\n", sep = "")
  cat("- Effect rows: ", if (is.data.frame(x$effectEvidence)) nrow(x$effectEvidence) else 0L, "\n", sep = "")
  cat("- Coefficient rows: ", if (is.data.frame(x$coefficientTable)) nrow(x$coefficientTable) else 0L, "\n", sep = "")

  invisible(x)
}
