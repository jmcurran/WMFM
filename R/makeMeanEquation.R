#' Construct a fitted-mean equation from model coefficients
#'
#' Builds an explicit equation for a single fitted mean using the model's
#' coefficient vector and the corresponding design row from
#' \code{model.matrix()}. This is designed to show how each fitted mean is
#' constructed from the regression table coefficients.
#'
#' @param m A fitted model object, typically an \code{lm} or \code{glm}.
#' @param oneRowDf A one-row data frame containing predictor values for which
#'   to construct the equation. Column names must match the model terms.
#' @param label A character label to place on the left-hand side of the equation.
#'
#' @return A length-1 character string containing the equation.
#'
#' @examples
#' df = data.frame(y = rnorm(8), A = factor(rep(c("a","b"), each = 4)))
#' mod = lm(y ~ A, data = df)
#' oneRow = data.frame(A = factor("b", levels = levels(df$A)))
#' makeMeanEquation(mod, oneRow, "Mean(A=b)")
#'
#' @importFrom stats model.matrix terms coef delete.response
#'
#' @export
makeMeanEquation = function(m, oneRowDf, label) {

  tt = delete.response(terms(m))
  X  = model.matrix(tt, data = oneRowDf)
  b  = coef(m)

  common = intersect(colnames(X), names(b))
  X = X[, common, drop = FALSE]
  b = b[common]

  termNames = colnames(X)
  prettyNames = sub("^\\(Intercept\\)$", "Intercept", termNames)

  contrib = as.numeric(X[1, ]) * as.numeric(b)
  keep    = abs(contrib) > 1e-12

  isGlm = inherits(m, "glm")
  isBinom = isGlm && identical(m$family$family, "binomial") && identical(m$family$link, "logit")

  fmt3sf = function(x) format(signif(x, 3), trim = TRUE, scientific = FALSE)
  fmt2dp = function(x) format(round(x, 2), nsmall = 2, trim = TRUE, scientific = FALSE)

  pieces = paste0(
    "(", prettyNames[keep], " = ", vapply(contrib[keep], fmt3sf, character(1)), ")"
  )
  rhs = if (length(pieces) == 0) "0" else paste(pieces, collapse = " + ")

  eta = sum(contrib)

  if (isBinom) {
    p = plogis(eta)
    paste0(
      label, " = ", rhs,
      " = log-odds: ", fmt3sf(eta),
      "  =>  p = ", fmt2dp(p), " (≈ ", fmt3sf(p), ")"
    )
  } else {
    paste0(
      label, " = ", rhs,
      " = ", fmt2dp(eta), " (≈ ", fmt3sf(eta), ")"
    )
  }
}
