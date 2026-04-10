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
  mf = model.frame(m)
  response = names(mf)[1]
  predictors = names(mf)[-1]

  isGlm = inherits(m, "glm")
  isBinom = isGlm && identical(m$family$family, "binomial") && identical(m$family$link, "logit")

  if (length(predictors) == 0) {
    # Intercept-only: for binomial logit we can still provide both scales
    eta0 = as.numeric(coef(m)[1])
    prob0 = if (isBinom) plogis(eta0) else NA_real_

    grid = data.frame(.pred = "(Intercept-only)", stringsAsFactors = FALSE)
    grid$.eta = eta0
    grid$.prob = prob0
    grid$.fit = if (isBinom) prob0 else eta0

    return(list(
      response = response,
      predictors = character(0),
      grid = grid,
      mf = mf,
      scale = if (isBinom) "probability" else "mean"
    ))
  }

  grid = expand.grid(
    lapply(predictors, function(v) levels(mf[[v]])),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  names(grid) = predictors

  if (isGlm) {
    eta = predict(m, newdata = grid, type = "link")
    grid$.eta = as.numeric(eta)

    if (isBinom) {
      grid$.prob = as.numeric(plogis(grid$.eta))
      grid$.fit = grid$.prob
      scale = "probability"
    } else {
      # For other GLMs, keep response-scale fit, but still retain eta
      grid$.prob = NA_real_
      grid$.fit = as.numeric(predict(m, newdata = grid, type = "response"))
      scale = "response"
    }

  } else {
    grid$.eta = NA_real_
    grid$.prob = NA_real_
    grid$.fit = as.numeric(predict(m, newdata = grid))
    scale = "mean"
  }

  list(
    response = response,
    predictors = predictors,
    grid = grid,
    mf = mf,
    scale = scale
  )
}
