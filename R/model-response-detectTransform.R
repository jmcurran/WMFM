#' Detect a response transformation from an expression string
#'
#' Attempts to detect a common response transformation from a text expression.
#' This is intended for lightweight UI labeling (e.g., "log", "sqrt") rather
#' than for full parsing of arbitrary R code.
#'
#' Recognised transformations (returned values):
#' \itemize{
#'   \item \code{"log"} for expressions starting with \code{log(}
#'   \item \code{"log1p"} for expressions starting with \code{log1p(}
#'   \item \code{"sqrt"} for expressions starting with \code{sqrt(}
#' }
#'
#' If no recognised transformation is detected, returns \code{NULL}.
#'
#' @param expr A character string containing an expression.
#'
#' @return A single character string naming the detected transformation, or
#'   \code{NULL} if none is detected.
#'
#' @examples
#' WMFM:::detectRespTransform("log(y)")
#' WMFM:::detectRespTransform(" log1p(count) ")
#' WMFM:::detectRespTransform("sqrt(x)")
#' WMFM:::detectRespTransform("y")
#'
#' @keywords internal
detectRespTransform = function(expr) {

  if (is.null(expr)) {
    return(NULL)
  }

  expr = trimws(as.character(expr))

  if (!nzchar(expr)) {
    return(NULL)
  }

  # Normalise whitespace for detection
  exprNoSpace = gsub("\\s+", "", expr)

  if (grepl("^log1p\\(", exprNoSpace)) {
    return("log1p")
  }

  if (grepl("^log\\(", exprNoSpace)) {
    return("log")
  }

  if (grepl("^sqrt\\(", exprNoSpace)) {
    return("sqrt")
  }

  NULL
}
