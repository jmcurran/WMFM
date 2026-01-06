#' Extract factor predictors from a fitted model
#'
#' Returns the names of predictor variables in a fitted model that are
#' factors in the supplied data set.
#'
#' @param model A fitted model object (e.g. \code{lm}, \code{glm}).
#' @param data A data frame containing the variables used to fit the model.
#'
#' @return A character vector of factor predictor names. May be empty.
#'
#' @examples
#' df = data.frame(
#'   y = rpois(10, 3),
#'   site = factor(rep(c("A", "B"), each = 5)),
#'   depth = rnorm(10)
#' )
#' mod = glm(y ~ site + depth, family = poisson, data = df)
#' getFactorPredictors(mod, df)
#'
#' @export
getFactorPredictors = function(model, data) {
  trm = terms(model)
  termLabels = attr(trm, "term.labels")

  if (length(termLabels) == 0) {
    return(character(0))
  }

  preds = unique(
    unlist(
      lapply(termLabels, function(lbl) {
        all.vars(as.formula(paste("~", lbl)))
      })
    )
  )

  preds = preds[preds %in% names(data)]

  preds[
    vapply(preds, function(v) {
      is.factor(data[[v]])
    }, logical(1))
  ]
}
