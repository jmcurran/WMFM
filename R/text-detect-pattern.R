#' Detect pattern in text
#'
#' @param x Character text.
#' @param pattern Regex pattern.
#'
#' @return Logical.
#' @export
detectWmfmPattern = function(x, pattern) {
  if (is.na(x)) return(NA)
  grepl(pattern, x, ignore.case = TRUE)
}
