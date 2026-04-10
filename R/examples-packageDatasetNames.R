#' List datasets available in an installed package
#'
#' Looks up the datasets reported by \code{utils::data(package = pkg)}
#' for a single installed package.
#'
#' @param pkg A package name.
#'
#' @return A sorted character vector of dataset names. Returns
#'   \code{character(0)} if the package name is empty, the package is not
#'   installed, or no datasets are found.
#'
#' @keywords internal
getPackageDatasetNames = function(pkg) {
  pkg = trimws(pkg %||% "")

  if (!nzchar(pkg) || !requireNamespace(pkg, quietly = TRUE)) {
    return(character(0))
  }

  out = tryCatch(
    utils::data(package = pkg),
    error = function(e) {
      NULL
    }
  )

  if (is.null(out) || is.null(out$results) || nrow(out$results) == 0) {
    return(character(0))
  }

  sort(unique(out$results[, "Item"]))
}
