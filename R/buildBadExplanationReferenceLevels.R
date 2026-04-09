#' Build reference-level context for factor predictors
#'
#' @param x A `wmfmModel` object.
#'
#' @return A character scalar describing factor predictors and their reference
#'   levels.
#' @keywords internal
buildBadExplanationReferenceLevels = function(x) {

  factorPredictors = tryCatch(
    getFactorPredictors(x$model, x$data),
    error = function(e) {
      character(0)
    }
  )

  if (length(factorPredictors) < 1L) {
    return("Factor reference levels: none")
  }

  details = vapply(factorPredictors, function(varName) {
    varData = x$data[[varName]]
    levelsVec = levels(varData)
    referenceLevel = if (length(levelsVec) > 0L) levelsVec[[1]] else "unknown"

    paste0(
      varName,
      ": reference level = ",
      referenceLevel,
      "; levels = ",
      paste(levelsVec, collapse = ", ")
    )
  }, character(1))

  paste(c("Factor reference levels:", details), collapse = "\n")
}
