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

#' Build the loading examples choice label
#'
#' @return A named character vector for the temporary example selector choice.
#' @keywords internal
buildLoadingExampleChoice = function() {
  c("Loading examples..." = "")
}

#' Build the startup package dataset pending status message
#'
#' @return A character string for the package dataset status display.
#' @keywords internal
buildPackageDatasetPendingScanStatus = function() {
  "Dataset choices will appear once the package scan has finished."
}

#' Build the startup notification message
#'
#' @return A character string for the startup notification.
#' @keywords internal
buildStartupDataChoicesMessage = function() {
  "Preparing built-in examples and installed-package datasets."
}

#' Build the example ready status message
#'
#' @return A character string for the example load status display.
#' @keywords internal
buildExampleReadyStatus = function() {
  "Choose a built-in example if you want the app to load a complete worked setup."
}

#' Build the no package datasets status message
#'
#' @return A character string for the package dataset status display.
#' @keywords internal
buildNoPackageDatasetsStatus = function() {
  "No package datasets are available yet."
}

#' Build the package list updating status message
#'
#' @return A character string for the package scan status display.
#' @keywords internal
buildPackageListUpdatingStatus = function() {
  "Updating the package list."
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

#' Build the package dataset checking status message
#'
#' @param packageName Name of the package being checked.
#'
#' @return A character string for the package dataset status display.
#' @keywords internal
buildPackageDatasetCheckingStatus = function(packageName) {
  paste0("Checking datasets in ", packageName, ".")
}

#' Build the package dataset loading notification message
#'
#' @param packageName Name of the package being scanned.
#'
#' @return A character string for the package dataset loading notification.
#' @keywords internal
buildPackageDatasetFindingMessage = function(packageName) {
  paste0("Finding datasets in ", packageName, ".")
}

#' Build the missing s20x package dataset status message
#'
#' @return A character string for the package dataset status display.
#' @keywords internal
buildS20xPackageMissingStatus = function() {
  "The s20x package is not installed."
}

#' Build the missing s20x package dataset choice label
#'
#' @return A character string for the disabled dataset choice label.
#' @keywords internal
buildS20xPackageMissingChoiceLabel = function() {
  "s20x is not installed"
}

#' Build the package dataset empty status message
#'
#' @param packageName Name of the package that has no datasets.
#'
#' @return A character string for the package dataset status display.
#' @keywords internal
buildPackageDatasetEmptyStatus = function(packageName) {
  paste0("No datasets were found in ", packageName, ".")
}

#' Build the package dataset empty choice label
#'
#' @return A character string for the disabled dataset choice label.
#' @keywords internal
buildPackageDatasetEmptyChoiceLabel = function() {
  "No datasets found"
}

#' Build the package dataset found status message
#'
#' @param packageName Name of the package that was scanned.
#' @param datasetNames Character vector of dataset names found in the package.
#'
#' @return A character string for the package dataset status display.
#' @keywords internal
buildPackageDatasetFoundStatus = function(packageName, datasetNames) {
  paste0(
    "Found ",
    length(datasetNames),
    " dataset",
    if (length(datasetNames) == 1) "" else "s",
    " in ",
    packageName,
    "."
  )
}

