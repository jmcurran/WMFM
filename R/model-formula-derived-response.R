#' Replace an inline response transformation with a recorded derived response
#'
#' When a user has created a derived response variable, the model formula should
#' use the derived variable name rather than retaining the same transformation as
#' an inline left-hand side expression. This keeps the fitted model connected to
#' the stored transformation metadata used for back-transformation.
#'
#' @param formulaText Character scalar model formula text.
#' @param responseVar Character scalar selected response variable.
#' @param variableTransformations Named list of derived-variable transformation
#'   records.
#'
#' @return Character scalar formula text, possibly with the left-hand side
#'   replaced by `responseVar`.
#' @importFrom stats as.formula
#' @keywords internal
substituteDerivedResponseInFormula = function(
  formulaText,
  responseVar,
  variableTransformations = NULL
) {
  formulaText = trimws(as.character(formulaText %||% ""))[1]
  responseVar = trimws(as.character(responseVar %||% ""))[1]

  if (!nzchar(formulaText) || !nzchar(responseVar)) {
    return(formulaText)
  }

  if (!is.list(variableTransformations) || is.null(variableTransformations[[responseVar]])) {
    return(formulaText)
  }

  record = variableTransformations[[responseVar]]
  recordRhs = trimws(as.character(record$rhs %||% ""))[1]

  if (!nzchar(recordRhs)) {
    return(formulaText)
  }

  parsedFormula = tryCatch(
    as.formula(formulaText),
    error = function(e) {
      NULL
    }
  )

  if (is.null(parsedFormula) || length(parsedFormula) < 3L) {
    return(formulaText)
  }

  lhsText = deparseOneLine(parsedFormula[[2]])

  if (!formulaExpressionsMatch(lhsText, recordRhs)) {
    return(formulaText)
  }

  rhsText = deparseOneLine(parsedFormula[[3]])
  paste(responseVar, "~", rhsText)
}

#' Test whether two formula expressions are textually equivalent
#'
#' @param first Character scalar expression text.
#' @param second Character scalar expression text.
#'
#' @return Logical scalar.
#' @keywords internal
formulaExpressionsMatch = function(first, second) {
  first = trimws(as.character(first %||% ""))[1]
  second = trimws(as.character(second %||% ""))[1]

  if (!nzchar(first) || !nzchar(second)) {
    return(FALSE)
  }

  firstParsed = tryCatch(
    parse(text = first)[[1]],
    error = function(e) {
      NULL
    }
  )
  secondParsed = tryCatch(
    parse(text = second)[[1]],
    error = function(e) {
      NULL
    }
  )

  if (is.null(firstParsed) || is.null(secondParsed)) {
    return(FALSE)
  }

  identical(
    removeExpressionWhitespace(deparseOneLine(firstParsed)),
    removeExpressionWhitespace(deparseOneLine(secondParsed))
  )
}

#' Remove whitespace from expression text for conservative comparison
#'
#' @param expressionText Character scalar expression text.
#'
#' @return Character scalar.
#' @keywords internal
removeExpressionWhitespace = function(expressionText) {
  gsub("[[:space:]]+", "", trimws(as.character(expressionText %||% ""))[1])
}
