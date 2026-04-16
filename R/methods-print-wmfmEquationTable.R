#' Print a deterministic equation table
#'
#' Prints one teaching case at a time, preserving separate scale-specific lines
#' where available.
#'
#' @param x A `wmfmEquationTable` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmEquationTable = function(
    x,
    ...
) {

  lines = formatWmfmEquationTableLines(x)

  if (length(lines) == 0) {
    cat("No equations available.\n")
    return(invisible(x))
  }

  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
