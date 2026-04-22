#' Compare WMFM objects
#'
#' Generic for comparing WMFM objects.
#'
#' @param x An object to compare.
#' @param y Optional second object to compare.
#' @param ... Additional arguments passed to methods.
#'
#' @return Method specific comparison object.
#' @export
compare = function(x, y = NULL, ...) {
  UseMethod("compare")
}
