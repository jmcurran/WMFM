#' Return the download extension for an analysis recipe
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return Either `"qmd"` for package data or `"zip"` for uploaded data.
#'
#' @keywords internal
analysisRecipeDownloadExtension = function(recipe) {
  validateAnalysisRecipe(recipe)

  if (identical(recipe$data$source %||% "unknown", "upload")) {
    return("zip")
  }

  "qmd"
}

#' Write a Quarto analysis document
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#' @param path Output path for the generated `.qmd` file.
#'
#' @return The normalised output path, invisibly.
#'
#' @keywords internal
writeAnalysisRecipeQuarto = function(recipe, path) {
  validateAnalysisRecipe(recipe)

  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("A non-empty Quarto output path is required.", call. = FALSE)
  }

  writeLines(renderAnalysisRecipeCoreQuarto(recipe), path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Write the downloadable analysis artifact
#'
#' Package-data analyses are written as a single Quarto document. Uploaded-data
#' analyses are written as a ZIP file containing the Quarto document and a
#' portable CSV copy of the analysis data.
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#' @param analysisData Data frame used to fit the model.
#' @param path Output path supplied by the download handler.
#'
#' @return The output path, invisibly.
#'
#' @keywords internal
writeAnalysisRecipeDownload = function(recipe, analysisData, path) {
  validateAnalysisRecipe(recipe)

  if (!identical(recipe$data$source %||% "unknown", "upload")) {
    writeAnalysisRecipeQuarto(recipe, path)
    return(invisible(path))
  }

  if (!is.data.frame(analysisData)) {
    stop("Uploaded-data analysis downloads require the fitted analysis data.", call. = FALSE)
  }

  bundleDirectory = tempfile("wmfm-analysis-")
  dataDirectory = file.path(bundleDirectory, "data")
  dir.create(dataDirectory, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bundleDirectory, recursive = TRUE, force = TRUE), add = TRUE)

  portableRecipe = recipe
  portableRecipe$data$uploadedFileName = "analysis_data.csv"

  quartoPath = file.path(bundleDirectory, "wmfm_analysis.qmd")
  dataPath = file.path(dataDirectory, "analysis_data.csv")

  writeAnalysisRecipeQuarto(portableRecipe, quartoPath)
  utils::write.csv(analysisData, dataPath, row.names = FALSE, na = "")

  oldDirectory = setwd(bundleDirectory)
  on.exit(setwd(oldDirectory), add = TRUE)

  zipStatus = utils::zip(
    zipfile = normalizePath(path, winslash = "/", mustWork = FALSE),
    files = c("wmfm_analysis.qmd", file.path("data", "analysis_data.csv")),
    flags = "-r9X"
  )

  if (!identical(as.integer(zipStatus), 0L) || !file.exists(path)) {
    stop("WMFM could not create the analysis download ZIP file.", call. = FALSE)
  }

  invisible(path)
}
