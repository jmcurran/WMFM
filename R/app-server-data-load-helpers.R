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


#' Build status text for a successfully loaded example.
#'
#' @param exampleName Character scalar example name.
#'
#' @return A character scalar status message.
#'
#' @keywords internal
buildLoadedExampleStatus = function(exampleName) {
  paste0(
    "Loaded example: ",
    exampleName,
    ". The data, research question, and model settings are ready on the Model tab."
  )
}

#' Build message shown when data is required before opening model help.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildLoadDataFirstMessage = function() {
  "Load a data set first."
}

#' Build title for the package data description modal.
#'
#' @param datasetName Character scalar dataset name.
#'
#' @return A character scalar modal title.
#'
#' @keywords internal
buildDataDescriptionTitle = function(datasetName = "") {
  if (!is.null(datasetName) && nzchar(datasetName)) {
    return(paste0("Data description: ", datasetName))
  }

  "Data description"
}

#' Build title for the user data context modal.
#'
#' @return A character scalar modal title.
#'
#' @keywords internal
buildProvideDataContextTitle = function() {
  "Provide data context"
}

#' Build help text for the user data context modal.
#'
#' @return A character scalar help text.
#'
#' @keywords internal
buildProvideDataContextHelpText = function() {
  "Describe the dataset in a way that will help the model explanation. For example, explain what the variables mean, how the data were collected, and what research question you want to answer."
}

#' Build placeholder text for the user data context modal.
#'
#' @return A character scalar placeholder text.
#'
#' @keywords internal
buildProvideDataContextPlaceholder = function() {
  "Example: Each row is a student. pass is 0/1. test is a score out of 20. attendance is days attended. We want to understand how test and attendance relate to passing."
}

#' Build message shown after saving user data context.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildDataContextSavedMessage = function() {
  "Data context saved."
}

#' Build message shown after clearing user data context.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildDataContextClearedMessage = function() {
  "Data context cleared."
}

