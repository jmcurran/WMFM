#' Coerce a WMFM factor predictor without retaining ordered contrasts
#'
#' Variables placed in the Factors bucket are treated as nominal predictors.
#' Ordered factors are therefore converted through character values so that
#' their ordered class and polynomial contrasts are removed.
#'
#' @param value A vector to treat as a nominal factor.
#'
#' @return An unordered factor.
#'
#' @keywords internal
#' @noRd
coerceWmfmFactor = function(value) {
  if (is.ordered(value)) {
    return(factor(as.character(value)))
  }

  if (is.factor(value)) {
    return(value)
  }

  factor(value)
}
