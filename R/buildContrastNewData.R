#' Build a newdata grid for factor-only contrasts
#'
#' Constructs a \code{newData} data frame suitable for generating predictions
#' or contrasts that vary one target factor across its levels while holding
#' all other factor predictors constant at user-specified values.
#'
#' The function returns:
#' \itemize{
#'   \item \code{newData}: One row per level of \code{targetFactor}.
#'   \item \code{targetLevels}: The levels of \code{targetFactor} used in \code{newData}.
#' }
#'
#' @param mf A model frame (typically \code{model.frame(m)}) that contains the
#'   variables referenced by the fitted model.
#' @param factorPreds Character vector of factor predictor names present in
#'   \code{mf}.
#' @param targetFactor A single string naming the factor predictor to vary.
#' @param condValues Named list (or named vector) giving the conditioning value
#'   (level) to hold each non-target factor predictor at. Names should correspond
#'   to entries in \code{factorPreds}. The \code{targetFactor} entry is ignored
#'   if present.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{newData}}{A data frame with one row per level of \code{targetFactor}.}
#'   \item{\code{targetLevels}}{A character vector of the target factor levels.}
#' }
#'
#' @keywords internal
buildContrastNewData = function(mf, factorPreds, targetFactor, condValues) {

  if (!is.character(targetFactor) || length(targetFactor) != 1) {
    stop("targetFactor must be a single character string.")
  }

  if (!targetFactor %in% factorPreds) {
    stop("targetFactor must be an element of factorPreds.")
  }

  if (!targetFactor %in% names(mf)) {
    stop("targetFactor was not found in mf.")
  }

  if (!is.factor(mf[[targetFactor]])) {
    stop("targetFactor column in mf must be a factor.")
  }

  targetLevels = levels(mf[[targetFactor]])
  if (length(targetLevels) == 0) {
    stop("targetFactor has no levels.")
  }

  # Pre-size to the required number of rows
  newData = data.frame(.row = seq_along(targetLevels))
  newData[[targetFactor]] = factor(targetLevels, levels = targetLevels)

  otherFactors = setdiff(factorPreds, targetFactor)

  if (length(otherFactors) > 0) {

    if (is.null(condValues)) {
      condValues = list()
    }

    if (!is.list(condValues)) {
      condValues = as.list(condValues)
    }

    missingCond = setdiff(otherFactors, names(condValues))
    if (length(missingCond) > 0) {
      stop(
        paste0(
          "Missing condValues for: ",
          paste(missingCond, collapse = ", ")
        )
      )
    }

    for (v in otherFactors) {

      if (!v %in% names(mf)) {
        stop(paste0("Variable '", v, "' was not found in mf."))
      }

      if (!is.factor(mf[[v]])) {
        stop(paste0("Variable '", v, "' in mf must be a factor."))
      }

      levs = levels(mf[[v]])
      chosen = condValues[[v]]

      if (length(chosen) != 1 || !is.character(chosen) || !nzchar(chosen)) {
        stop(paste0("condValues[['", v, "']] must be a single non-empty string."))
      }

      if (!chosen %in% levs) {
        stop(
          paste0(
            "condValues[['", v, "']] = '", chosen,
            "' is not a level of '", v, "'."
          )
        )
      }

      newData[[v]] = factor(rep(chosen, length(targetLevels)), levels = levs)
    }
  }

  # Drop placeholder row column
  newData$.row = NULL

  list(
    newData = newData,
    targetLevels = targetLevels
  )
}
