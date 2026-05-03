#' Build message for a failed delimited file read.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildDelimitedFileReadFailedMessage = function() {
  "Failed to read file with the chosen separator."
}

#' Build message for an RDA or RData file without a data frame.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildNoDataFrameInRdaMessage = function() {
  "No data frame in RDA file."
}

#' Build message for an unsupported upload file type.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildUnsupportedUploadFileTypeMessage = function() {
  "Unsupported file type. Please upload CSV, TXT, or RDA."
}

#' Build message for a missing custom text separator.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildMissingSeparatorMessage = function() {
  "Please specify a separator."
}

#' Build message for a package dataset loading failure.
#'
#' @param datasetName Character scalar dataset name.
#' @param packageName Character scalar package name.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildPackageDatasetLoadFailedMessage = function(datasetName, packageName) {
  paste0("Could not load dataset '", datasetName, "' from package '", packageName, "'.")
}

#' Build message for a selected object that is not a data frame.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildSelectedObjectNotDataFrameMessage = function() {
  "Selected object is not a data frame."
}

#' Build message for missing example selection.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildChooseExampleFirstMessage = function() {
  "Choose an example first."
}
