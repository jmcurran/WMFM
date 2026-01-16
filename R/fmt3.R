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
