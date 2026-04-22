#' Score WMFM objects
#'
#' Generic for scoring WMFM objects.
#'
#' @param x An object to score.
#' @param ... Additional arguments passed to methods.
#'
#' @return Method-specific scored output.
#' @export
score = function(x, ...) {
  UseMethod("score")
}
