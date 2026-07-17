#' Build the student-facing WMFM confidence-interval table
#'
#' Reproduces the confidence-interval table used by WMFM for a fitted model,
#' including interpreted fitted quantities and effects when the model structure
#' supports them.
#'
#' @param model A fitted `lm` or supported `glm` object.
#' @param level Confidence level between zero and one.
#'
#' @return A data frame containing the WMFM confidence-interval table.
#'
#' @export
modelConfidenceIntervals = function(model, level = 0.95) {
  ciData = buildModelConfidenceIntervalData(
    model = model,
    level = level,
    numericReference = chooseModelNumericReference(model = model)
  )

  ciData$table
}
