#' Choose the numeric reference used for teaching summaries
#'
#' Selects whether numeric predictors should be anchored at 0 or at their
#' sample means when building teaching-oriented summaries such as fitted-value
#' confidence-interval rows. The current rule is intentionally simple:
#' if every numeric predictor has an observed range that includes 0, use 0;
#' otherwise use the sample mean.
#'
#' @param model Optional fitted model object. Used only when \code{mf}
#'   is not supplied.
#' @param mf Optional model frame. If omitted, it is computed from
#'   \code{model}.
#' @param predictorNames Optional character vector of predictor names.
#'
#' @return A single string, either \code{"zero"} or \code{"mean"}.
#' @keywords internal
#'
#' @importFrom stats model.frame
chooseModelNumericReference = function(model = NULL, mf = NULL, predictorNames = NULL) {

  if (is.null(mf)) {
    if (is.null(model)) {
      stop("Supply either `model` or `mf`.", call. = FALSE)
    }

    mf = model.frame(model)
  }

  if (is.null(predictorNames)) {
    predictorNames = names(mf)[-1]
  }

  if (length(predictorNames) == 0) {
    return("zero")
  }

  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(numericNames) == 0) {
    return("zero")
  }

  zeroInRange = vapply(
    numericNames,
    function(varName) {
      x = mf[[varName]]
      x = x[!is.na(x)]

      if (length(x) == 0) {
        return(TRUE)
      }

      min(x) <= 0 && max(x) >= 0
    },
    logical(1)
  )

  if (all(zeroInRange)) {
    return("zero")
  }

  "mean"
}
