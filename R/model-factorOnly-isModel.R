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
