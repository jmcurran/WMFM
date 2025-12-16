#' Build fitted means for factor-only predictor models
#'
#' Creates a full grid of factor-level combinations for all predictors in the
#' fitted model frame and computes fitted values for each combination.
#'
#' For \code{glm} models, fitted values are returned on the response scale
#' via \code{predict(type = "response")}. For \code{lm} models, fitted values
#' are returned from \code{predict()}.
#'
#' @param m A fitted model object, typically an \code{lm} or \code{glm}.
#'
#' @return A list with components:
#' \describe{
#'   \item{response}{The response variable name.}
#'   \item{predictors}{Character vector of predictor names (in model frame order).}
#'   \item{grid}{A data frame of predictor level combinations with an added
#'     numeric column \code{.fit}.}
#'   \item{mf}{The model frame used for extracting factor levels.}
#' }
#'
#' @examples
#' df = data.frame(
#'   y = rnorm(12),
#'   A = factor(rep(c("a","b"), each = 6)),
#'   B = factor(rep(c("u","v","w"), times = 4))
#' )
#' mod = lm(y ~ A * B, data = df)
#' out = makeFittedMeansData(mod)
#' head(out$grid)
#'
#' @importFrom stats model.frame predict coef
#'
#' @export
makeFittedMeansData = function(m) {
  mf = stats::model.frame(m)
  response = names(mf)[1]
  predictors = names(mf)[-1]

  if (length(predictors) == 0) {
    return(list(
      response = response,
      predictors = character(0),
      grid = data.frame(.pred = "(Intercept-only)", .fit = as.numeric(stats::coef(m)[1])),
      mf = mf
    ))
  }

  grid = expand.grid(
    lapply(predictors, function(v) levels(mf[[v]])),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  names(grid) = predictors

  fit = if (inherits(m, "glm")) {
    stats::predict(m, newdata = grid, type = "response")
  } else {
    stats::predict(m, newdata = grid)
  }

  grid$.fit = as.numeric(fit)

  list(
    response = response,
    predictors = predictors,
    grid = grid,
    mf = mf
  )
}
