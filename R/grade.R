#' Grade WMFM objects
#'
#' Generic for grading user-written explanations against a prepared WMFM model
#' context.
#'
#' @param x An object to grade against.
#' @param ... Additional arguments passed to methods.
#'
#' @return A method-specific graded output.
#' @export
grade = function(x, ...) {
  UseMethod("grade")
}
