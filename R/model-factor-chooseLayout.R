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
