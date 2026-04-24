#' Format a numeric value for deterministic equation output
#'
#' Formats a numeric coefficient for student-facing deterministic equation
#' rendering using fixed decimal places and ordinary decimal notation.
#'
#' @param x A numeric scalar.
#' @param digits Integer number of decimal places. Defaults to 2.
#'
#' @return A character scalar.
#'
#' @keywords internal
formatEquationNumber = function(x, digits = 2) {

  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop("`x` must be a single non-missing numeric value.", call. = FALSE)
  }

  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits)) {
    stop("`digits` must be a single non-missing numeric value.", call. = FALSE)
  }

  digits = as.integer(digits)

  format(round(x, digits), nsmall = digits, trim = TRUE, scientific = FALSE)
}


#' Collapse coefficient components into a displayable expression term
#'
#' Builds one deterministic algebra term from one or more coefficient components.
#' The result carries its own sign so later joining can avoid malformed patterns
#' such as "+ (-1.23)".
#'
#' @param values Numeric vector of coefficient components.
#' @param variable Optional character scalar naming the retained variable.
#' @param digits Integer number of decimal places. Defaults to 2.
#'
#' @return A character scalar.
#'
#' @keywords internal
buildEquationComponentText = function(values, variable = NULL, digits = 2) {

  if (!is.numeric(values) || any(is.na(values))) {
    stop("`values` must be a numeric vector with no missing values.", call. = FALSE)
  }

  values = as.numeric(values)

  if (length(values) == 0) {
    return("")
  }

  if (length(values) == 1) {
    prefix = if (values[1] < 0) "- " else ""
    expr = formatEquationNumber(abs(values[1]), digits = digits)
  } else {
    firstSign = if (values[1] < 0) -1 else 1
    inside = formatEquationNumber(abs(values[1]), digits = digits)

    if (length(values) > 1) {
      for (i in 2:length(values)) {
        nextSign = if (values[i] < 0) -1 else 1
        joiner = if (nextSign == firstSign) " + " else " - "
        inside = paste0(
          inside,
          joiner,
          formatEquationNumber(abs(values[i]), digits = digits)
        )
      }
    }

    prefix = if (firstSign < 0) "- " else ""
    expr = paste0("(", inside, ")")
  }

  if (!is.null(variable)) {
    expr = paste0(expr, " * ", variable)
  }

  paste0(prefix, expr)
}


#' Join signed deterministic algebra terms into one right-hand side
#'
#' Joins pre-signed algebra fragments into one plain-text expression while
#' preserving ordinary sign formatting.
#'
#' @param pieces Character vector of algebra fragments.
#'
#' @return A character scalar.
#'
#' @keywords internal
joinEquationPieces = function(pieces) {

  pieces = pieces[nzchar(pieces)]

  if (length(pieces) == 0) {
    return("0")
  }

  out = pieces[1]

  if (length(pieces) > 1) {
    for (i in 2:length(pieces)) {
      piece = pieces[i]

      if (grepl("^- ", piece)) {
        out = paste0(out, " - ", sub("^- ", "", piece))
      } else {
        out = paste0(out, " + ", piece)
      }
    }
  }

  out
}
