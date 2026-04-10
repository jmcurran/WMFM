#' Add a derived variable to a data frame from a single assignment line
#'
#' Validates and evaluates a single assignment statement such as
#' \code{t = 1:nrow(data)} or \code{month = factor(rep(1:12, 12))}.
#' The RHS is evaluated in a safe environment created by \code{makeSafeEvalEnv()},
#' so only the data columns plus a small allowlist of base functions are available.
#'
#' The derived variable is appended to the data frame as a new column.
#' Scalars are recycled to the number of rows; otherwise the result must have
#' length \code{nrow(data)}.
#'
#' @param data A data frame to modify.
#' @param txt A length-1 character string containing one assignment statement.
#' @param allowOverwrite Logical; if \code{FALSE} (default), refuse to overwrite an existing column.
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{ok}: logical, whether the operation succeeded.
#'   \item \code{msg}: character status message.
#'   \item \code{data}: the updated data frame (if ok), otherwise the original data.
#'   \item \code{name}: the new variable name (if ok).
#' }
#'
#' @examples
#' df = data.frame(passengers = 1:144)
#' res = addDerivedVariableToData(df, "t = 1:nrow(data)")
#' res$ok
#' names(res$data)
#'
#' res2 = addDerivedVariableToData(res$data, "month = factor(rep(1:12, 12))")
#' table(res2$data$month)
#'
#' @export
addDerivedVariableToData = function(data, txt, allowOverwrite = FALSE) {
  if (!is.data.frame(data)) {
    return(list(ok = FALSE, msg = "data must be a data frame.", data = data))
  }

  txt = trimws(txt)
  if (txt == "") {
    return(list(ok = FALSE, msg = "Enter an assignment like: t = 1:nrow(data)", data = data))
  }

  parsed = parseSingleAssignment(txt)
  if (!isTRUE(parsed$ok)) {
    return(list(ok = FALSE, msg = parsed$msg, data = data))
  }

  newName = parsed$name
  rhs = parsed$rhs

  if (!allowOverwrite && newName %in% names(data)) {
    return(list(ok = FALSE, msg = paste0("'", newName, "' already exists in the data."), data = data))
  }

  evalEnv = makeSafeEvalEnv(data)
  value = tryCatch(eval(rhs, envir = evalEnv), error = function(e) e)

  if (inherits(value, "error")) {
    return(list(ok = FALSE, msg = paste("Error:", conditionMessage(value)), data = data))
  }

  n = nrow(data)
  if (!(length(value) %in% c(1, n))) {
    return(list(
      ok = FALSE,
      msg = paste0("Result length must be 1 or ", n, " (got ", length(value), ")."),
      data = data
    ))
  }

  if (length(value) == 1) {
    value = rep(value, n)
  }

  data[[newName]] = value
  list(ok = TRUE, msg = paste0("Added variable: ", newName), data = data, name = newName)
}
