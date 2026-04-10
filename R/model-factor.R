#' Choose fitted-means table layout for 1–3 factor predictors
#'
#' Implements the layout rules for displaying fitted means when all predictors
#' are factors:
#' \itemize{
#'   \item 1 factor: one-way table (single column of fitted means).
#'   \item 2 factors: two-way table with the factor having the most levels on rows.
#'   \item 3 factors: a set of two-way tables split by the factor with the fewest
#'     levels. Within each two-way table, the factor with the most levels is on rows
#'     and the remaining factor is on columns.
#' }
#'
#' @param mf A model frame (as returned by \code{model.frame(m)}).
#' @param predictors Character vector of predictor names (columns in \code{mf}).
#'
#' @return A list describing the layout:
#' \describe{
#'   \item{type}{One of \code{"oneWay"}, \code{"twoWay"}, \code{"threeWay"}, or \code{"other"}.}
#'   \item{rowVar}{Row factor name (for \code{"oneWay"} and \code{"twoWay"} and \code{"threeWay"}).}
#'   \item{colVar}{Column factor name (for \code{"twoWay"} and \code{"threeWay"}).}
#'   \item{splitVar}{Split factor name (for \code{"threeWay"} only).}
#' }
#'
#' @examples
#' df = data.frame(
#'   y = rnorm(24),
#'   A = factor(rep(c("a1","a2"), each = 12)),
#'   B = factor(rep(paste0("b", 1:4), times = 6)),
#'   C = factor(rep(paste0("c", 1:6), times = 4))
#' )
#' mod = lm(y ~ A * B * C, data = df)
#' mf = model.frame(mod)
#' chooseFactorLayout(mf, c("A","B","C"))
#'
#' @importFrom stats model.frame
#'
#' @export
chooseFactorLayout = function(mf, predictors) {
  nLevels = vapply(predictors, function(v) nlevels(mf[[v]]), integer(1))

  if (length(predictors) == 1) {
    return(list(type = "oneWay", rowVar = predictors[1]))
  }

  if (length(predictors) == 2) {
    ord = order(nLevels, decreasing = TRUE)
    return(list(
      type = "twoWay",
      rowVar = predictors[ord[1]],
      colVar = predictors[ord[2]]
    ))
  }

  if (length(predictors) == 3) {
    ord = order(nLevels, decreasing = FALSE)
    splitVar = predictors[ord[1]]

    other = setdiff(predictors, splitVar)
    otherLevels = nLevels[other]

    ord2 = order(otherLevels, decreasing = TRUE)

    rowVar = other[ord2[1]]
    colVar = other[ord2[2]]

    return(list(
      type = "threeWay",
      splitVar = splitVar,
      rowVar = rowVar,
      colVar = colVar
    ))
  }

  list(type = "other")
}


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
