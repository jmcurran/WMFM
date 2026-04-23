#' Parse a weight entered as text
#'
#' Parses a user-entered weight string into a numeric value. Supports:
#' \itemize{
#'   \item Decimal numbers (e.g., \code{"0.25"}, \code{"2"}, \code{"-1.5"})
#'   \item Simple fractions (e.g., \code{"1/2"}, \code{" 3 / 4 "})
#' }
#'
#' If parsing fails, the function returns \code{NA_real_}.
#'
#' @param x A character string containing a weight specification.
#'
#' @return A numeric scalar if parsing succeeds; otherwise \code{NA_real_}.
#'
#' @examples
#' WMFM:::parseWeightText("0.5")
#' WMFM:::parseWeightText("1/2")
#' WMFM:::parseWeightText(" 3 / 4 ")
#' WMFM:::parseWeightText("not a number")
#'
#' @keywords internal
parseWeightText = function(x) {

  if (is.null(x)) {
    return(NA_real_)
  }

  x = trimws(as.character(x))

  if (!nzchar(x)) {
    return(NA_real_)
  }

  # Fraction form: a/b (allow spaces)
  if (grepl("/", x, fixed = TRUE)) {

    parts = strsplit(gsub("\\s+", "", x), "/", fixed = TRUE)[[1]]

    if (length(parts) != 2) {
      return(NA_real_)
    }

    num = suppressWarnings(as.numeric(parts[1]))
    den = suppressWarnings(as.numeric(parts[2]))

    if (!is.finite(num) || !is.finite(den) || den == 0) {
      return(NA_real_)
    }

    return(num / den)
  }

  # Plain numeric
  val = suppressWarnings(as.numeric(x))
  if (!is.finite(val)) {
    return(NA_real_)
  }

  val
}
