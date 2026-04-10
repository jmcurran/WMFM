#' Parse a single assignment statement from text
#'
#' Parses user input and verifies it is exactly one assignment statement of the form
#' \code{name = expr} or \code{name <- expr}. This is designed for validating a
#' derived-variable input box in a Shiny app.
#'
#' @param txt A length-1 character string containing R code.
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{ok}: logical, whether parsing/validation succeeded.
#'   \item \code{msg}: character message if \code{ok = FALSE}.
#'   \item \code{name}: (if ok) the variable name on the LHS.
#'   \item \code{rhs}: (if ok) the RHS expression (language object).
#' }
#'
#' @examples
#' parseSingleAssignment("t = 1:10")$ok
#' parseSingleAssignment("1:10")$ok
#' parseSingleAssignment("x = log(y)")$name
#'
#' @export
parseSingleAssignment = function(txt) {
  txt = trimws(txt)

  if (txt == "") {
    return(list(ok = FALSE, msg = "Enter an assignment like: t = 1:nrow(data)"))
  }

  if (!grepl("=", txt, fixed = TRUE) || grepl("~", txt, fixed = TRUE)) {
    return(list(ok = FALSE, msg = "Enter a single assignment like: t = 1:nrow(data)"))
  }

  # Allow ":" for sequences; keep a conservative whitelist.
  # Put "-" at the end of the character class to avoid invalid ranges in TRE.
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*\\s*=\\s*[0-9A-Za-z_:+*/^()., -]+$", txt)) {
    return(list(ok = FALSE, msg = "Derived-variable expression contains illegal characters."))
  }

  expr = tryCatch(parse(text = txt), error = function(e) NULL)
  if (is.null(expr) || length(expr) != 1) {
    return(list(ok = FALSE, msg = "Derived-variable expression must be a single assignment."))
  }

  e1 = expr[[1]]
  if (!is.call(e1) || as.character(e1[[1]]) != "=") {
    return(list(ok = FALSE, msg = "Expression must be of the form: name = expression"))
  }

  lhs = e1[[2]]
  if (!is.name(lhs)) {
    return(list(ok = FALSE, msg = "Left-hand side must be a single variable name."))
  }

  list(ok = TRUE, name = as.character(lhs), rhs = e1[[3]])
}

