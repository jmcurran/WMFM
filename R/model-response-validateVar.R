#' Validate a response variable for a given model type
#'
#' Determines whether a candidate response variable is compatible with the
#' model-fitting logic implemented in the package.
#'
#' This function exists to prevent unsupported modelling choices such as
#' multi-level categorical responses for logistic regression (multinomial
#' models are not implemented).
#'
#' Supported response types:
#' \itemize{
#'   \item \strong{lm}: numeric responses or 2-level factors
#'   \item \strong{logistic}: binary responses only
#'   \item \strong{poisson}: non-negative integer counts
#' }
#'
#' @param data A data frame containing the response variable.
#' @param responseVar A character string naming the response variable.
#' @param modelType A character string: \code{"lm"}, \code{"logistic"},
#'   or \code{"poisson"}.
#'
#' @return A list with components:
#' \describe{
#'   \item{ok}{Logical; whether the response is supported.}
#'   \item{reason}{Character string explaining the decision.}
#' }
#'
#' @importFrom stats na.omit setNames
#'
#' @keywords internal
validateResponseVar = function(data, responseVar, modelType) {

  if (!is.data.frame(data)) {
    return(list(ok = FALSE, reason = "No data available."))
  }

  if (!nzchar(responseVar) || !(responseVar %in% names(data))) {
    return(list(ok = FALSE, reason = "No valid response variable selected."))
  }

  y = data[[responseVar]]

  nDistinct = function(x) {
    length(unique(na.omit(x)))
  }

  if (identical(modelType, "lm")) {

    if (is.numeric(y)) {
      return(list(ok = TRUE, reason = "Numeric response."))
    }

    if (is.factor(y) && nlevels(y) == 2) {
      return(list(ok = TRUE, reason = "2-level factor response."))
    }

    if (is.logical(y)) {
      return(list(ok = TRUE, reason = "Logical response."))
    }

    return(list(
      ok = FALSE,
      reason = "Linear regression supports numeric or 2-level factor responses only."
    ))
  }

  if (identical(modelType, "logistic")) {

    if (is.factor(y)) {
      if (nlevels(y) == 2) {
        return(list(ok = TRUE, reason = "Binary factor response."))
      }
      return(list(
        ok = FALSE,
        reason = "Multinomial logistic regression is not implemented."
      ))
    }

    if (is.character(y)) {
      if (nDistinct(y) == 2) {
        return(list(ok = TRUE, reason = "Binary character response."))
      }
      return(list(
        ok = FALSE,
        reason = "Logistic regression requires exactly two response values."
      ))
    }

    if (is.logical(y)) {
      return(list(ok = TRUE, reason = "Logical response."))
    }

    if (is.numeric(y)) {
      uy = unique(na.omit(y))
      if (length(uy) == 0) {
        return(list(ok = FALSE, reason = "Response has no non-missing values."))
      }
      if (all(uy %in% c(0, 1))) {
        return(list(ok = TRUE, reason = "Numeric 0/1 response."))
      }
      return(list(
        ok = FALSE,
        reason = "Numeric logistic responses must be coded 0/1."
      ))
    }

    return(list(
      ok = FALSE,
      reason = "Logistic regression requires a binary response."
    ))
  }

  if (identical(modelType, "poisson")) {

    if (!is.numeric(y)) {
      return(list(
        ok = FALSE,
        reason = "Poisson regression requires a numeric count response."
      ))
    }

    yy = na.omit(y)

    if (length(yy) == 0) {
      return(list(ok = FALSE, reason = "Response has no non-missing values."))
    }

    if (any(yy < 0) || any(yy %% 1 != 0)) {
      return(list(
        ok = FALSE,
        reason = "Poisson regression requires non-negative integer counts."
      ))
    }

    return(list(ok = TRUE, reason = "Count response."))
  }

  list(ok = FALSE, reason = "Unknown model type.")
}
