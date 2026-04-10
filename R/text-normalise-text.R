#' Normalise text for comparison
#'
#' Lowercases and removes extra whitespace.
#'
#' @param x Character text.
#'
#' @return Character string.
#' @export
normaliseWmfmText = function(x) {
  if (is.na(x)) return(NA_character_)

  x = tolower(x)
  x = gsub("[[:space:]]+", " ", x)
  trimws(x)
}
