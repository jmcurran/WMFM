#' Null-coalescing operator
#'
#' Returns \code{y} when \code{x} is \code{NULL}; otherwise returns \code{x}.
#' This is useful for providing defaults when optional values may be missing.
#'
#' @param x A value that may be \code{NULL}.
#' @param y A default value to use when \code{x} is \code{NULL}.
#'
#' @return If \code{x} is \code{NULL}, returns \code{y}; otherwise returns \code{x}.
#'
#' @name nullCoalesce
#' @aliases \\%||\\%
#' @keywords internal
NULL

#' @rdname nullCoalesce
`%||%` = function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
}
