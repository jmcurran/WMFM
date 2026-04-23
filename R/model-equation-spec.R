#' Build a deterministic equation specification from a fitted model
#'
#' Extracts the core structural information needed by a deterministic equation
#' renderer. The returned specification is intended to separate model
#' inspection from later case enumeration and string rendering.
#'
#' The specification includes:
#' \itemize{
#'   \item model family and link
#'   \item response-variable name
#'   \item predictor metadata, including factor levels and reference levels
#'   \item coefficient estimates
#'   \item interaction metadata based on the fitted formula terms
#' }
#'
#' This helper is intentionally conservative. It aims to describe the fitted
#' model clearly, not to perform symbolic algebra.
#'
#' @param model A fitted model object, typically of class \code{lm} or
#'   \code{glm}.
#'
#' @return A list of class \code{"wmfmEquationSpec"}.
#'
#' @importFrom stats coef family model.frame terms
#'
#' @keywords internal
buildEquationSpec = function(model) {

  if (missing(model) || is.null(model)) {
    stop("`model` must be supplied.", call. = FALSE)
  }

  if (!(inherits(model, "lm") || inherits(model, "glm"))) {
    stop("`model` must inherit from `lm` or `glm`.", call. = FALSE)
  }

  mf = stats::model.frame(model)

  if (ncol(mf) < 1) {
    stop("Could not recover a valid model frame.", call. = FALSE)
  }

  responseName = names(mf)[1]
  predictorNames = names(mf)[-1]
  trm = stats::terms(model)
  termLabels = attr(trm, "term.labels")

  if (inherits(model, "glm")) {
    familyInfo = stats::family(model)
    familyName = familyInfo$family
    linkName = familyInfo$link
  } else {
    familyName = "gaussian"
    linkName = "identity"
  }

  predictorInfo = lapply(
    predictorNames,
    function(predictorName) {
      x = mf[[predictorName]]

      if (is.factor(x)) {
        list(
          name = predictorName,
          type = "factor",
          levels = levels(x),
          reference = levels(x)[1]
        )
      } else {
        list(
          name = predictorName,
          type = "numeric",
          levels = NULL,
          reference = NULL
        )
      }
    }
  )

  names(predictorInfo) = predictorNames

  coefficientValues = stats::coef(model)
  coefficientTable = data.frame(
    term = names(coefficientValues),
    estimate = as.numeric(coefficientValues),
    stringsAsFactors = FALSE
  )

  interactionLabels = termLabels[grepl(":", termLabels, fixed = TRUE)]

  interactionInfo = lapply(
    interactionLabels,
    function(label) {
      vars = all.vars(as.formula(paste("~", label)))
      types = vapply(
        vars,
        function(varName) {
          if (!(varName %in% predictorNames)) {
            return("unknown")
          }

          predictorInfo[[varName]]$type
        },
        character(1)
      )

      list(
        term = label,
        vars = vars,
        type = paste(types, collapse = ":")
      )
    }
  )

  notation = buildGlmTeachingNotation(model)

  out = list(
    family = familyName,
    link = linkName,
    responseName = responseName,
    predictorNames = predictorNames,
    predictors = predictorInfo,
    coefficients = coefficientTable,
    termLabels = termLabels,
    interactions = interactionInfo,
    notation = notation
  )

  class(out) = c("wmfmEquationSpec", class(out))
  out
}
