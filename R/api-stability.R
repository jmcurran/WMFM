#' Assess stability of WMFM objects
#'
#' Generic for assessing stability of WMFM objects.
#'
#' @param x An object to assess.
#' @param ... Additional arguments passed to methods.
#'
#' @return Method-specific stability output.
#' @export
stability = function(x, ...) {
  UseMethod("stability")
}
