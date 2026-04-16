#' Build deterministic teaching cases from an equation specification
#'
#' Enumerates the displayed teaching cases implied by a deterministic equation
#' specification. A case represents one condition to be shown later by the
#' rendering layer, such as a general equation, one factor level, or one
#' factor-level combination.
#'
#' The current policy is intentionally simple:
#' \itemize{
#'   \item intercept-only and numeric-only models produce one general case
#'   \item models with one or more factor predictors produce one case per factor
#'     level combination
#'   \item numeric predictors are retained symbolically within each case rather
#'     than substituted
#' }
#'
#' This helper decides which cases to show, but it does not render any algebra
#' or response-scale expressions.
#'
#' @param spec A deterministic equation specification, typically produced by
#'   \code{buildEquationSpec()}.
#'
#' @return A list of class \code{"wmfmEquationCases"} containing one element
#'   per displayed case.
#'
#' @keywords internal
buildEquationCases = function(spec) {

  if (missing(spec) || is.null(spec)) {
    stop("`spec` must be supplied.", call. = FALSE)
  }

  if (!inherits(spec, "wmfmEquationSpec")) {
    stop("`spec` must inherit from `wmfmEquationSpec`.", call. = FALSE)
  }

  predictorInfo = spec$predictors

  if (length(predictorInfo) == 0) {
    out = list(
      list(
        caseId = "overall",
        label = "Overall",
        factorValues = list(),
        retainedNumeric = character(0)
      )
    )

    class(out) = c("wmfmEquationCases", class(out))
    return(out)
  }

  predictorTypes = vapply(
    predictorInfo,
    function(x) {
      x$type
    },
    character(1)
  )

  factorNames = names(predictorTypes)[predictorTypes == "factor"]
  numericNames = names(predictorTypes)[predictorTypes == "numeric"]

  if (length(factorNames) == 0) {
    out = list(
      list(
        caseId = "general",
        label = "General",
        factorValues = list(),
        retainedNumeric = numericNames
      )
    )

    class(out) = c("wmfmEquationCases", class(out))
    return(out)
  }

  factorLevels = lapply(
    factorNames,
    function(factorName) {
      predictorInfo[[factorName]]$levels
    }
  )
  names(factorLevels) = factorNames

  factorGrid = do.call(
    expand.grid,
    c(factorLevels, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  )

  out = lapply(
    seq_len(nrow(factorGrid)),
    function(i) {
      row = factorGrid[i, , drop = FALSE]
      factorValues = as.list(row)
      names(factorValues) = names(row)

      label = paste(
        vapply(
          names(factorValues),
          function(varName) {
            paste0(varName, " = ", factorValues[[varName]])
          },
          character(1)
        ),
        collapse = ", "
      )

      caseId = paste(
        vapply(
          names(factorValues),
          function(varName) {
            paste0(varName, "_", factorValues[[varName]])
          },
          character(1)
        ),
        collapse = "__"
      )

      list(
        caseId = caseId,
        label = label,
        factorValues = factorValues,
        retainedNumeric = numericNames
      )
    }
  )

  class(out) = c("wmfmEquationCases", class(out))
  out
}
