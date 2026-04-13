#' Construct a fitted-mean equation from model coefficients
#'
#' Builds an explicit equation for a single fitted mean using the model's
#' coefficient vector and the corresponding design row from
#' \code{model.matrix()}. This is designed to show how each fitted mean is
#' constructed from the regression table coefficients.
#'
#' For GLMs, the regression coefficients combine to form the linear predictor
#' (often written \eqn{\eta}). This function shows that linear predictor and
#' then shows the back-transformation to the mean on the response scale.
#'
#' @param m A fitted model object, typically an \code{lm} or \code{glm}.
#' @param oneRowDf A one-row data frame containing predictor values for which
#'   to construct the equation. Column names must match the model terms.
#' @param label A character label to place on the left-hand side of the equation.
#'
#' @return A length-1 character string containing the equation (may include
#'   newline characters for multi-step GLM working).
#'
#' @examples
#' df = data.frame(y = rnorm(8), A = factor(rep(c("a","b"), each = 4)))
#' mod = lm(y ~ A, data = df)
#' oneRow = data.frame(A = factor("b", levels = levels(df$A)))
#' makeMeanEquation(mod, oneRow, "Mean(A=b)")
#'
#' @importFrom stats model.matrix terms coef delete.response family plogis
#'
#' @export
makeMeanEquation = function(m, oneRowDf, label) {

  tt = delete.response(terms(m))
  X = model.matrix(tt, data = oneRowDf)
  b = coef(m)

  common = intersect(colnames(X), names(b))
  X = X[, common, drop = FALSE]
  b = b[common]

  termNames = colnames(X)
  prettyNames = sub("^\\(Intercept\\)$", "Intercept", termNames)

  contrib = as.numeric(X[1, ]) * as.numeric(b)
  keep = abs(contrib) > 1e-12

  fmt3sf = function(x) format(signif(x, 3), trim = TRUE, scientific = FALSE)
  fmt2dp = function(x) format(round(x, 2), nsmall = 2, trim = TRUE, scientific = FALSE)

  pieces = paste0(
    "(", prettyNames[keep], " = ", vapply(contrib[keep], fmt3sf, character(1)), ")"
  )
  rhs = if (length(pieces) == 0) "0" else paste(pieces, collapse = " + ")

  eta = sum(contrib)

  isGlm = inherits(m, "glm")

  if (!isGlm) {
    return(
      paste0(
        label, " = ", rhs,
        " = ", fmt2dp(eta), " (\u2248 ", fmt3sf(eta), ")"
      )
    )
  }

  fam = family(m)$family
  link = family(m)$link
  invLink = family(m)$linkinv

  # Line 1: always show the linear predictor
  line1 = paste0(
    label, ": linear predictor (eta) = ", rhs,
    " = ", fmt2dp(eta), " (\u2248 ", fmt3sf(eta), ")"
  )

  # Line 2: interpret eta + back-transform
  if (fam == "binomial" && link == "logit") {
    successProbLabel = formatBinomialProbabilityLabel(m, outcome = "success")
    failureProbLabel = formatBinomialProbabilityLabel(m, outcome = "failure")
    successOddsLabel = formatBinomialOddsLabel(m, outcome = "success")
    failureOddsLabel = formatBinomialOddsLabel(m, outcome = "failure")

    successProb = plogis(eta)
    failureProb = 1 - successProb
    successOdds = exp(eta)
    failureOdds = exp(-eta)

    line2 = paste0(
      "eta = logit(", successProbLabel, ")  =>  ",
      successOddsLabel, " = exp(eta) = ", fmt2dp(successOdds), " (\u2248 ", fmt3sf(successOdds), ")",
      "\n",
      failureOddsLabel, " = exp(-eta) = ", fmt2dp(failureOdds), " (\u2248 ", fmt3sf(failureOdds), ")",
      "\n",
      successProbLabel, " = exp(eta) / (1 + exp(eta)) = ", fmt2dp(successProb), " (\u2248 ", fmt3sf(successProb), ")",
      "\n",
      failureProbLabel, " = 1 / (1 + exp(eta)) = ", fmt2dp(failureProb), " (\u2248 ", fmt3sf(failureProb), ")"
    )
  } else if (fam == "poisson" && link == "log") {
    mu = invLink(eta)  # exp(eta)
    line2 = paste0(
      "eta = log(mean)  =>  mean = exp(eta) = ", fmt2dp(mu), " (\u2248 ", fmt3sf(mu), ")"
    )
  } else {
    mu = invLink(eta)
    line2 = paste0(
      "eta = ", link, "(mean)  =>  mean = ", link, "^{-1}(eta) = ",
      fmt2dp(mu), " (\u2248 ", fmt3sf(mu), ")"
    )
  }

  paste(c(line1, line2), collapse = "\n")
}
