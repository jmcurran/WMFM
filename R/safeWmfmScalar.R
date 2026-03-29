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
