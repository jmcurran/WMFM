#' Null-coalescing operator
#'
#' Returns \code{y} when \code{x} is \code{NULL}; otherwise returns \code{x}.
#' This is useful for providing defaults when optional values may be missing.
#'
#' @param x A value that may be \code{NULL}.
#' @param y A default value to use when \code{x} is \code{NULL}.
#'
#' @return If \code{x} is \code{NULL}, returns \code{y}; otherwise returns \code{x}.
#'
#' @name nullCoalesce
#' @aliases \\%||\\%
#' @keywords internal
NULL

#' @rdname nullCoalesce
`%||%` = function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
}


#' Safely convert a scalar-like object to character text
#'
#' Converts a scalar-like object to a single character value suitable for prompt
#' construction. This helper handles common edge cases such as `NULL`, empty
#' vectors, factors, date-time objects, and single-element lists.
#'
#' @param x Object to convert.
#' @param naString Character string to return for missing or empty values.
#'   Defaults to `"NA"`.
#' @param trim Logical. Should surrounding whitespace be removed?
#'   Defaults to `TRUE`.
#' @param singleLine Logical. Should internal whitespace be collapsed to a single
#'   space? Defaults to `FALSE`.
#'
#' @return A character scalar.
#' @keywords internal
safeWmfmScalar = function(
    x,
    naString = "NA",
    trim = TRUE,
    singleLine = FALSE
) {

  if (!is.character(naString) || length(naString) != 1 || is.na(naString)) {
    stop("`naString` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!is.logical(trim) || length(trim) != 1 || is.na(trim)) {
    stop("`trim` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(singleLine) || length(singleLine) != 1 || is.na(singleLine)) {
    stop("`singleLine` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.null(x) || length(x) == 0) {
    return(naString)
  }

  while (is.list(x) && length(x) == 1) {
    x = x[[1]]
    if (is.null(x) || length(x) == 0) {
      return(naString)
    }
  }

  x = x[[1]]

  if (inherits(x, "POSIXt")) {
    out = format(x, tz = "UTC", usetz = TRUE)
  } else if (inherits(x, "Date")) {
    out = format(x)
  } else if (is.factor(x)) {
    out = as.character(x)
  } else {
    out = as.character(x)
  }

  if (length(out) == 0 || is.na(out) || identical(out, "NA")) {
    return(naString)
  }

  if (isTRUE(trim)) {
    out = trimws(out)
  }

  if (!nzchar(out)) {
    return(naString)
  }

  if (isTRUE(singleLine)) {
    out = gsub("[[:space:]]+", " ", out)
  }

  out
}


#' Create a safe evaluation environment for derived-variable expressions
#'
#' Builds an environment that contains the columns of \code{data} and a small
#' allowlist of safe base functions. This is intended for evaluating the
#' right-hand side (RHS) of user-entered derived-variable assignments in a
#' Shiny app without exposing powerful functions like file I/O or system calls.
#'
#' The returned environment contains:
#' \itemize{
#'   \item All columns of \code{data} as symbols.
#'   \item A symbol \code{data} bound to the full data frame.
#'   \item Only the allowlisted functions (from \code{base}) in the parent environment.
#' }
#'
#' @param data A data frame.
#'
#' @return An environment suitable for \code{eval(rhs, envir = env)}.
#'
#' @examples
#' df = data.frame(x = 1:5)
#' env = makeSafeEvalEnv(df)
#' eval(parse(text = "log(x)"), envir = env)
#' eval(parse(text = "factor(rep(1:2, length.out = nrow(data)))"), envir = env)
#'
#' @export
makeSafeEvalEnv = function(data) {
  env = new.env(parent = emptyenv())

  # data columns
  for (nm in names(data)) {
    env[[nm]] = data[[nm]]
  }
  env[["data"]] = data

  # allowlisted functions (whatever you already add)
  env[["asin"]]    = base::asin
  env[["arcsin"]]    = base::asin
  env[["factor"]] = base::factor
  env[["I"]]    = base::I
  env[["log"]]    = base::log
  env[["log1p"]]    = base::log1p
  env[["nrow"]] = base::nrow
  env[["rep"]]    = base::rep
  env[["seq_len"]] = base::seq_len
  env[["sqrt"]] = base::sqrt
  # etc.

  # IMPORTANT: allow ":" for sequences like 1:144
  env[[":"]] = get(":", baseenv())

  env
}
