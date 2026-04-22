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


#' Check whether a fitted model has only factor predictors
#'
#' Determines whether all predictors in a fitted linear or generalised
#' linear model are factors in the supplied data set.
#'
#' Intercept-only models are not considered factor-only.
#'
#' @param model A fitted model object (e.g. \code{lm}, \code{glm}).
#' @param data A data frame containing the variables used to fit the model.
#'
#' @return Logical scalar. Returns \code{TRUE} if all predictors are factors,
#'   otherwise \code{FALSE}.
#'
#' @examples
#' df = data.frame(
#'   y = rpois(20, 5),
#'   site = factor(rep(c("A", "B"), each = 10))
#' )
#' mod = glm(y ~ site, family = poisson, data = df)
#' isFactorOnlyModel(mod, df)
#'
#' @export
isFactorOnlyModel = function(model, data) {
  if (is.null(model) || is.null(data)) {
    return(FALSE)
  }

  trm = terms(model)
  termLabels = attr(trm, "term.labels")

  if (length(termLabels) == 0) {
    return(FALSE)
  }

  all(
    vapply(termLabels, function(lbl) {
      vars = all.vars(as.formula(paste("~", lbl)))
      all(
        vapply(vars, function(v) {
          is.factor(data[[v]])
        }, logical(1))
      )
    }, logical(1))
  )
}


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
