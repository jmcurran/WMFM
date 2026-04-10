#' Count words in text
#'
#' @param x Character text.
#'
#' @return Integer.
#' @export
countWmfmWords = function(x) {
  if (is.na(x) || !nzchar(x)) return(0L)
  length(strsplit(x, "[[:space:]]+")[[1]])
}
