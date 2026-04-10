#' Detect factor-only predictor models
#'
#' Determines whether a fitted model has *only factor predictors* in its
#' model frame (i.e., every predictor column is a factor). This is used to
#' switch the "Fitted equations" view into an ANOVA-style "Fitted means" view.
#'
#' The response is not checked for being a factor; in typical "fitted means"
#' usage the response is numeric and predictors are factors. Intercept-only
#' models (no predictors) return \code{FALSE}.
#'
#' @param m A fitted model object, typically an \code{lm} or \code{glm}.
#'
#' @return A logical scalar. \code{TRUE} if all predictors in
#'   \code{model.frame(m)} are factors; otherwise \code{FALSE}.
#'
#' @examples
#' df = data.frame(y = rnorm(12), A = factor(rep(letters[1:3], each = 4)))
#' mod = lm(y ~ A, data = df)
#' isFactorOnlyPredictorModel(mod)
#'
#' @importFrom stats model.frame
#'
#' @export
isFactorOnlyPredictorModel = function(m) {
  if (is.null(m)) {
    return(FALSE)
  }

  mf = stats::model.frame(m)

  # Need at least one predictor column (response + >=1 predictor)
  if (ncol(mf) < 2) {
    return(FALSE)
  }

  preds = mf[, -1, drop = FALSE]
  all(vapply(preds, is.factor, logical(1)))
}
