#' List installed packages that contain datasets
#'
#' Returns the names of installed packages for which
#' \code{utils::data(package = ...)} reports at least one dataset.
#' The result is sorted alphabetically and is useful for populating
#' a package selector in the app's Load Data tab.
#'
#' @return A character vector of installed package names that appear to
#'   contain one or more datasets.
#'
#' @keywords internal
getInstalledPackagesWithData = function() {
  pkgs = rownames(installed.packages())

  hasData = vapply(
    pkgs,
    function(pkg) {
      out = tryCatch(
        utils::data(package = pkg),
        error = function(e) {
          NULL
        }
      )

      !is.null(out) &&
        !is.null(out$results) &&
        nrow(out$results) > 0
    },
    logical(1)
  )

  sort(unique(pkgs[hasData]))
}
