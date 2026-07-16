#' Resolve data provenance for an analysis recipe
#'
#' Package-backed WMFM examples retain their package and dataset provenance,
#' even though the example loader uses the upload-facing controls in the app.
#' File-backed examples remain portable data downloads.
#'
#' @param dataSource Current app data-source value.
#' @param packageName Current package selection.
#' @param datasetName Current package dataset selection.
#' @param uploadedFileName Original uploaded file name, when available.
#' @param loadedExample Optional loaded-example metadata.
#'
#' @return A list containing resolved data-source metadata.
#'
#' @keywords internal
#' @noRd
resolveAnalysisRecipeDataMetadata = function(
  dataSource,
  packageName = "",
  datasetName = "",
  uploadedFileName = "",
  loadedExample = NULL
) {
  metadata = list(
    source = dataSource %||% "unknown",
    packageName = packageName %||% "",
    datasetName = datasetName %||% "",
    uploadedFileName = uploadedFileName %||% ""
  )

  if (is.null(loadedExample)) {
    return(metadata)
  }

  exampleSpec = loadedExample$spec %||% list()
  exampleSource = exampleSpec$dataSource %||% "file"

  if (identical(exampleSource, "package")) {
    return(list(
      source = "package",
      packageName = exampleSpec$dataPackage %||% "",
      datasetName = exampleSpec$dataObject %||% "",
      uploadedFileName = ""
    ))
  }

  list(
    source = "upload",
    packageName = "",
    datasetName = "",
    uploadedFileName = basename(exampleSpec$data %||% uploadedFileName %||% "analysis_data.csv")
  )
}

#' Build the core analysis recipe after a successful model fit
#'
#' @param model Fitted `lm` or `glm` object.
#' @param dataSource Character scalar identifying the active data source.
#' @param packageName Optional package name for package data.
#' @param datasetName Optional data-set name.
#' @param uploadedFileName Optional original uploaded file name.
#' @param variableTransformations Recorded WMFM variable transformations.
#' @param factorVariables Variables explicitly treated as factors by WMFM.
#' @param responseTransformationMode Current response-transformation display mode.
#'
#' @return A `wmfmAnalysisRecipe` object.
#'
#' @keywords internal
buildAnalysisRecipeFromFit = function(
  model,
  dataSource,
  packageName = "",
  datasetName = "",
  uploadedFileName = "",
  variableTransformations = list(),
  factorVariables = character(0),
  responseTransformationMode = "both"
) {
  if (!inherits(model, c("lm", "glm"))) {
    stop("A fitted lm or glm object is required to build an analysis recipe.", call. = FALSE)
  }

  modelFormula = stats::formula(model)
  modelTerms = stats::terms(modelFormula)
  responseName = all.vars(modelFormula[[2]])[1]
  predictorNames = unique(all.vars(modelFormula[[3]]))
  modelFamily = stats::family(model)

  modelType = if (inherits(model, "glm")) {
    if (identical(modelFamily$family, "binomial")) {
      "logistic"
    } else if (identical(modelFamily$family, "poisson")) {
      "poisson"
    } else {
      "glm"
    }
  } else {
    "lm"
  }

  dataMetadata = list(
    source = dataSource %||% "unknown",
    packageName = packageName %||% "",
    datasetName = datasetName %||% "",
    uploadedFileName = uploadedFileName %||% ""
  )

  preparationMetadata = list(
    variableTransformations = variableTransformations %||% list(),
    factorVariables = unique(factorVariables %||% character(0)),
    responseTransformationMode = responseTransformationMode %||% "both"
  )

  modelMetadata = list(
    formula = paste(deparse(modelFormula), collapse = " "),
    modelType = modelType,
    family = modelFamily$family,
    link = modelFamily$link,
    response = responseName,
    predictors = predictorNames,
    termLabels = attr(modelTerms, "term.labels") %||% character(0)
  )

  newAnalysisRecipe(
    data = dataMetadata,
    preparation = preparationMetadata,
    model = modelMetadata,
    output = list(
      renderer = "quarto",
      documentFileName = "wmfm_analysis.qmd"
    )
  )
}
