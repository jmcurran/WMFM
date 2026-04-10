#' Extract text from WMFM output objects
#'
#' Converts model explanation or equation objects into plain text.
#'
#' @param x Object returned by WMFM functions.
#'
#' @return Character string.
#' @export
extractWmfmText = function(x) {
  if (is.null(x)) return(NA_character_)

  if (is.character(x)) {
    return(paste(x, collapse = "\n"))
  }

  tryCatch(
    paste(capture.output(print(x)), collapse = "\n"),
    error = function(e) NA_character_
  )
}
