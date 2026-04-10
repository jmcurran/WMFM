#' Identify factor-only predictors in a fitted model
#'
#' Extracts the predictor variables from a fitted model that are factors,
#' excluding the response variable. Predictor names are determined from the
#' model terms and matched against the columns in the supplied model frame.
#'
#' This helper is primarily used to identify categorical predictors that are
#' eligible for factor-only contrasts.
#'
#' @param m A fitted model object (e.g., from \code{lm()} or \code{glm()}).
#' @param mf A model frame corresponding to \code{m}, typically obtained via
#'   \code{model.frame(m)}.
#'
#' @return A character vector of predictor names that are factors in
#'   \code{mf}. Returns an empty character vector if no such predictors exist.
#'
#' @importFrom stats terms
#'
#' @examples
#' df = data.frame(
#'   y = rnorm(10),
#'   f = factor(rep(c("A", "B"), each = 5)),
#'   x = rnorm(10)
#' )
#'
#' m = lm(y ~ f + x, data = df)
#' mf = model.frame(m)
#'
#' WMFM:::getFactorOnlyPredictors(m, mf)
#'
#' @keywords internal
getFactorOnlyPredictors = function(m, mf) {

  trm = terms(m)

  preds = attr(trm, "term.labels")
  preds = as.character(preds)

  if (length(preds) == 0) {
    return(character(0))
  }

  isFactor = function(v) {
    is.factor(mf[[v]])
  }

  preds[sapply(preds, isFactor)]
}
