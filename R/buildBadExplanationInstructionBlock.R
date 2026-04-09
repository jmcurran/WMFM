#' Build explanation-specific instructions for bad explanation generation
#'
#' @param plan A plan object produced by `buildBadExplanationPlan()`.
#'
#' @return A character scalar.
#' @keywords internal
buildBadExplanationInstructionBlock = function(plan) {

  rows = vapply(seq_along(plan$explanationNames), function(i) {
    explanationName = plan$explanationNames[[i]]
    typesThisExplanation = plan$selectedTypes[[i]]
    severityThisExplanation = plan$severity[[explanationName]]

    paste0(
      "- ", explanationName,
      ": severity = ", severityThisExplanation,
      "; errorTypes = [\"",
      paste(typesThisExplanation, collapse = "\", \""),
      "\"]"
    )
  }, character(1))

  severityGuide = c(
    "Severity guide:",
    "- subtle: mostly plausible, with one meaningful mistake.",
    "- moderate: clearly flawed, with one major mistake or two smaller mistakes.",
    "- severe: multiple strong mistakes, but still coherent and realistic."
  )

  paste(c(severityGuide, "", "Generate these explanations:", rows), collapse = "\n")
}
