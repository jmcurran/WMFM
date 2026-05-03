#' Build the initial package scan status message
#'
#' @param initialPackageChoices Character vector of immediately available package
#'   choices.
#'
#' @return A character string for the package scan status display.
#' @keywords internal
buildInitialPackageScanStatus = function(initialPackageChoices) {
  if (length(initialPackageChoices) > 0) {
    return("Showing s20x now while other installed packages are checked.")
  }

  "Checking installed packages for datasets."
}

#' Build the default package dataset status message
#'
#' @return A character string for the package dataset status display.
#' @keywords internal
buildPackageDatasetChoiceStatus = function() {
  "Choose a package to see its available datasets."
}

#' Build the default example load status message
#'
#' @return A character string for the example load status display.
#' @keywords internal
buildInitialExampleLoadStatus = function() {
  "Loading the built-in examples."
}

#' Build the package scan completion status message
#'
#' @param packageNames Character vector of packages with datasets.
#' @param initialPackageChoices Character vector of package choices that were
#'   available before the full package scan completed.
#'
#' @return A character string, or `NULL` when no status message is needed.
#' @keywords internal
buildPackageScanCompleteStatus = function(packageNames, initialPackageChoices) {
  if (length(packageNames) == 0) {
    return("No installed packages with datasets were found.")
  }

  newPackageNames = setdiff(packageNames, initialPackageChoices)

  if (length(newPackageNames) == 0) {
    return(NULL)
  }

  paste0(
    "Found ",
    length(packageNames),
    " installed package",
    if (length(packageNames) == 1) "" else "s",
    " with datasets."
  )
}
