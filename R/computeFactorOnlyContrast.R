#' Compute a single contrast for factor-only models
#'
#' Computes an estimate and 95% confidence interval for a user-specified
#' contrast in a factor-only model, optionally using robust (sandwich)
#' variance estimators.
#'
#' The contrast is defined through a set of weights applied to the rows of
#' \code{newData}. For example, a pairwise contrast between level A and level B
#' can be represented with weights \code{c(1, -1, 0, ..., 0)}.
#'
#' For GLMs, inference is carried out on the linear predictor (link) scale and
#' then back-transformed for interpretation when possible:
#' \itemize{
#'   \item Poisson (log link): reports a mean ratio \eqn{exp(contrast)}.
#'   \item Binomial (logit link): reports an odds ratio \eqn{exp(contrast)}.
#' }
#'
#' @param model A fitted model object, typically an \code{lm} or \code{glm}.
#' @param newData A data frame whose rows define the conditions being contrasted.
#' @param weights Numeric vector of contrast weights, one per row of \code{newData}.
#' @param ciType One of \code{"standard"} or \code{"sandwich"}.
#' @param hcType Sandwich type passed to \code{sandwich::vcovHC()} (e.g. \code{"HC0"}, \code{"HC3"}).
#' @param level Confidence level (default 0.95).
#'
#' @return A list with estimates and confidence intervals on the link scale and
#'   (when applicable) an interpreted scale.
#'
#' @importFrom stats coef vcov model.matrix terms delete.response family qnorm qt df.residual
#' @importFrom sandwich vcovHC
#'
#' @export
computeFactorOnlyContrast = function(model,
                                     newData,
                                     weights,
                                     ciType = c("standard", "sandwich"),
                                     hcType = c("HC0", "HC3"),
                                     level = 0.95) {

  ciType = match.arg(ciType)
  hcType = match.arg(hcType)

  if (nrow(newData) == 0) {
    stop("newData must have at least one row.")
  }
  if (length(weights) != nrow(newData)) {
    stop("weights must have length equal to nrow(newData).")
  }

  tt = delete.response(terms(model))
  X = model.matrix(tt, data = newData)

  b = coef(model)
  common = intersect(colnames(X), names(b))
  X = X[, common, drop = FALSE]
  b = b[common]

  cVec = as.numeric(t(weights) %*% X)

  if (ciType == "sandwich") {
    V = vcovHC(model, type = hcType)
  } else {
    V = vcov(model)
  }

  V = V[common, common, drop = FALSE]

  estEta = as.numeric(cVec %*% b)
  seEta = sqrt(as.numeric(cVec %*% V %*% cVec))

  alpha = 1 - level

  isGlm = inherits(model, "glm")
  useT = (!isGlm) && (ciType == "standard")

  crit = if (useT) {
    qt(1 - alpha / 2, df = df.residual(model))
  } else {
    qnorm(1 - alpha / 2)
  }

  lowerEta = estEta - crit * seEta
  upperEta = estEta + crit * seEta

  out = list(
    ciType = ciType,
    hcType = hcType,
    level = level,
    estEta = estEta,
    seEta = seEta,
    lowerEta = lowerEta,
    upperEta = upperEta,
    interpreted = NULL
  )

  if (!isGlm) {
    out$interpreted = list(
      scale = "difference",
      label = "Difference in means",
      estimate = estEta,
      lower = lowerEta,
      upper = upperEta
    )
    return(out)
  }

  fam = family(model)$family
  link = family(model)$link

  if ((fam == "poisson" && link == "log") || (fam == "binomial" && link == "logit")) {
    out$interpreted = list(
      scale = if (fam == "poisson") "ratio" else "oddsRatio",
      label = if (fam == "poisson") "Mean ratio" else "Odds ratio",
      estimate = exp(estEta),
      lower = exp(lowerEta),
      upper = exp(upperEta)
    )
  } else {
    out$interpreted = list(
      scale = "link",
      label = paste0("Contrast on ", link, " scale"),
      estimate = estEta,
      lower = lowerEta,
      upper = upperEta
    )
  }

  out
}
