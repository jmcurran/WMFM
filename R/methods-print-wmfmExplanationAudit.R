#' Print a WMFM explanation audit
#'
#' @param x A `wmfmExplanationAudit` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmExplanationAudit = function(x, ...) {

  catLine = function(label, value) {
    cat("- ", label, ": ", value %||% "", "\n", sep = "")
  }

  cat("WMFM explanation audit\n")
  cat("---------------------\n")
  cat(x$transparencyNote %||% "", "\n\n", sep = "")

  if (is.list(x$overview) && length(x$overview) > 0) {
    cat("Overview\n")
    catLine("Response", x$overview$response)
    catLine("Predictors", paste(x$overview$predictors %||% character(0), collapse = ", "))
    catLine("Observations", x$overview$nObservations)
    catLine("Model family", x$overview$modelFamily)
    catLine("Link", x$overview$link)
    catLine("Dataset context available", x$overview$hasDatasetContext)
    catLine("Research question available", x$overview$hasResearchQuestion)
    cat("\n")
  }

  if (is.list(x$interpretationScale) && length(x$interpretationScale) > 0) {
    cat("Interpretation scale\n")
    catLine("Response expression", x$interpretationScale$responseExpression)
    catLine("Response transform", x$interpretationScale$responseTransform)
    catLine("Fitted-value scale", x$interpretationScale$fittedValueScale)
    catLine("Effect scale", x$interpretationScale$effectScale)
    catLine("Back-transformation", x$interpretationScale$backTransformation)
    cat("\n")
  }

  if (is.list(x$numericAnchor) && length(x$numericAnchor) > 0) {
    cat("Numeric anchors\n")
    catLine("Reference rule", x$numericAnchor$numericReference)
    catLine("Note", x$numericAnchor$note)

    if (is.data.frame(x$numericAnchor$table) && nrow(x$numericAnchor$table) > 0) {
      print(x$numericAnchor$table)
    } else {
      cat("(none)\n")
    }

    cat("\n")
  }

  if (is.data.frame(x$referenceLevels) && nrow(x$referenceLevels) > 0) {
    cat("Reference levels\n")
    print(x$referenceLevels)
    cat("\n")
  }

  if (is.list(x$confidenceIntervals) && length(x$confidenceIntervals) > 0) {
    cat("Confidence intervals\n")
    catLine("Level", x$confidenceIntervals$level)
    catLine("Mode", x$confidenceIntervals$mode)
    catLine(
      "Displayed scales",
      paste(x$confidenceIntervals$displayedScales %||% character(0), collapse = ", ")
    )
    catLine("Teaching note", x$confidenceIntervals$teachingNote)
    cat("\n")
  }

  if (is.data.frame(x$baselineEvidence)) {
    cat("Baseline evidence rows: ", nrow(x$baselineEvidence), "\n", sep = "")
  }

  if (is.data.frame(x$effectEvidence)) {
    cat("Effect evidence rows: ", nrow(x$effectEvidence), "\n", sep = "")
  }

  if (is.data.frame(x$coefficientTable)) {
    cat("Coefficient rows: ", nrow(x$coefficientTable), "\n", sep = "")
  }

  invisible(x)
}
