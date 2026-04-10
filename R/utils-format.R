#' Format a numeric value to three significant figures
#'
#' Formats numeric values for user-facing display using approximately three
#' significant figures, suppressing scientific notation where possible.
#'
#' @param x A numeric vector.
#'
#' @return A character vector of the same length as \code{x}.
#'
#' @examples
#' WMFM:::fmt3(c(0.001234, 12.3456, 12345.6))
#'
#' @keywords internal
fmt3 = function(x) {
  format(signif(x, 3), trim = TRUE, scientific = FALSE)
}


#' Format a numeric summary value for display
#'
#' Format a single numeric value for display in a summary table using a
#' significant-digits rule that aims to keep only the digits that carry useful
#' information. Integer values are shown without decimal places. Non-integer
#' values are rounded to a chosen number of significant digits.
#'
#' This is useful for compact summary tables where fixed decimal places can
#' produce distracting trailing zeros, for example showing `11` rather than
#' `11.000`.
#'
#' @param x A numeric scalar to format.
#' @param sigDigits Integer. The number of significant digits to keep for
#'   non-integer values. Defaults to 3.
#'
#' @returns A character scalar. Returns `NA_character_` when `x` has length 0 or
#'   is `NA`.
#'
#' @examples
#' formatSummaryValue(11)
#' formatSummaryValue(52.8767)
#' formatSummaryValue(68.5)
#' formatSummaryValue(0.012345, sigDigits = 2)
#'
#' @export
formatSummaryValue = function(x, sigDigits = 3) {
  if (length(x) == 0) {
    return(NA_character_)
  }

  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  if (length(x) != 1) {
    stop("x must be a numeric scalar.")
  }

  if (is.na(x)) {
    return(NA_character_)
  }

  if (!is.numeric(sigDigits) || length(sigDigits) != 1 || is.na(sigDigits)) {
    stop("sigDigits must be a single non-missing numeric value.")
  }

  sigDigits = as.integer(sigDigits)

  if (sigDigits < 1) {
    stop("sigDigits must be at least 1.")
  }

  if (isTRUE(all.equal(x, round(x)))) {
    return(format(round(x), trim = TRUE, scientific = FALSE))
  }

  format(signif(x, sigDigits), trim = TRUE, scientific = FALSE)
}
