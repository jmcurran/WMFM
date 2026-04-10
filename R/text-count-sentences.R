#' Count sentences in text
#'
#' @param x Character text.
#'
#' @return Integer.
#' @export
countWmfmSentences = function(x) {
  if (is.na(x)) return(NA_integer_)
  length(unlist(regmatches(x, gregexpr("[.!?]+", x))))
}
