#' Render the core Quarto sections for a WMFM analysis recipe
#'
#' Renders deterministic student-facing code for loading packages, loading the
#' data, preparing the analysis data, and fitting the selected model.
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return A character vector containing Quarto source lines.
#'
#' @keywords internal
renderAnalysisRecipeCoreQuarto = function(recipe) {
  validateAnalysisRecipe(recipe)

  c(
    "---",
    "title: \"WMFM analysis\"",
    "format: html",
    "execute:",
    "  echo: true",
    "---",
    "",
    "# Load packages",
    "",
    renderAnalysisRecipePackageChunk(recipe),
    "",
    "# Load the data",
    "",
    renderAnalysisRecipeDataChunk(recipe),
    "",
    "# Prepare the data",
    "",
    renderAnalysisRecipePreparationChunk(recipe),
    "",
    "# Fit the model",
    "",
    renderAnalysisRecipeModelChunk(recipe),
    renderAnalysisRecipeAnalysisQuarto(recipe)
  )
}

#' Render the package-loading code chunk
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipePackageChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  packageNames = "WMFM"
  if (identical(recipe$data$source, "package") && nzchar(recipe$data$packageName %||% "")) {
    packageNames = unique(c(packageNames, recipe$data$packageName))
  }

  codeLines = if (length(packageNames) == 0) {
    "# No additional packages are required for the core analysis."
  } else {
    paste0("library(", packageNames, ")")
  }

  renderAnalysisRecipeCodeChunk("load-packages", codeLines)
}

#' Render the data-loading code chunk
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeDataChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  dataSource = recipe$data$source %||% "unknown"

  if (identical(dataSource, "package")) {
    packageName = recipe$data$packageName %||% ""
    datasetName = recipe$data$datasetName %||% ""

    if (!nzchar(packageName) || !nzchar(datasetName)) {
      stop("Package data recipes require both a package and data-set name.", call. = FALSE)
    }

    codeLines = c(
      paste0(
        "data(list = ",
        encodeAnalysisRecipeString(datasetName),
        ", package = ",
        encodeAnalysisRecipeString(packageName),
        ")"
      ),
      paste0("analysisData = get(", encodeAnalysisRecipeString(datasetName), ")")
    )

    return(renderAnalysisRecipeCodeChunk("load-data", codeLines))
  }

  if (identical(dataSource, "upload")) {
    uploadedFileName = recipe$data$uploadedFileName %||% ""
    extension = tolower(sub("^.*\\.", "", uploadedFileName))

    if (!nzchar(uploadedFileName)) {
      stop("Uploaded-data recipes require the original uploaded file name.", call. = FALSE)
    }

    if (!identical(extension, "csv")) {
      stop(
        paste0(
          "Core report rendering currently supports package data and CSV uploads. ",
          "Portable loading for .",
          extension,
          " files has not yet been designed."
        ),
        call. = FALSE
      )
    }

    relativePath = file.path("data", basename(uploadedFileName))
    codeLines = c(
      "analysisData = read.table(",
      paste0("  ", encodeAnalysisRecipeString(relativePath), ","),
      "  sep = \",\",",
      "  header = TRUE,",
      "  stringsAsFactors = FALSE,",
      "  check.names = TRUE,",
      "  fill = TRUE",
      ")"
    )

    return(renderAnalysisRecipeCodeChunk("load-data", codeLines))
  }

  stop(
    paste0("Unsupported analysis recipe data source: ", dataSource, "."),
    call. = FALSE
  )
}

#' Render the data-preparation code chunk
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipePreparationChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  transformationRecords = normaliseVariableTransformations(
    recipe$preparation$variableTransformations %||% list()
  )
  factorVariables = unique(recipe$preparation$factorVariables %||% character(0))

  codeLines = character(0)
  transformedVariables = character(0)

  if (length(transformationRecords) > 0) {
    for (record in transformationRecords) {
      variableName = record$variable %||% ""
      rhsText = record$rhs %||% ""

      if (!nzchar(variableName) || !nzchar(rhsText)) {
        next
      }

      codeLines = c(
        codeLines,
        paste0(
          "analysisData$",
          variableName,
          " = with(analysisData, ",
          rhsText,
          ")"
        )
      )
      transformedVariables = c(transformedVariables, variableName)
    }
  }

  factorVariables = setdiff(factorVariables, transformedVariables)
  if (length(factorVariables) > 0) {
    factorLines = vapply(
      factorVariables,
      function(variableName) {
        paste0(
          "analysisData$",
          variableName,
          " = factor(analysisData$",
          variableName,
          ")"
        )
      },
      character(1)
    )
    codeLines = c(codeLines, factorLines)
  }

  if (length(codeLines) == 0) {
    codeLines = "# No additional data preparation is required."
  }

  renderAnalysisRecipeCodeChunk("prepare-data", codeLines)
}

#' Render the model-fitting code chunk
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeModelChunk = function(recipe) {
  validateAnalysisRecipe(recipe)

  formulaText = recipe$model$formula %||% ""
  modelType = recipe$model$modelType %||% ""

  if (!nzchar(formulaText)) {
    stop("The analysis recipe does not contain a model formula.", call. = FALSE)
  }

  if (identical(modelType, "lm")) {
    codeLines = c(
      "modelFit = lm(",
      paste0("  formula = ", formulaText, ","),
      "  data = analysisData",
      ")"
    )
  } else if (identical(modelType, "logistic")) {
    codeLines = c(
      "modelFit = glm(",
      paste0("  formula = ", formulaText, ","),
      "  data = analysisData,",
      "  family = binomial(link = \"logit\")",
      ")"
    )
  } else if (identical(modelType, "poisson")) {
    codeLines = c(
      "modelFit = glm(",
      paste0("  formula = ", formulaText, ","),
      "  data = analysisData,",
      "  family = poisson(link = \"log\")",
      ")"
    )
  } else {
    stop(
      paste0("Unsupported model type for core report rendering: ", modelType, "."),
      call. = FALSE
    )
  }

  renderAnalysisRecipeCodeChunk("fit-model", codeLines)
}

#' Render a labelled Quarto R code chunk
#'
#' @param label Character chunk label.
#' @param codeLines Character vector of R source lines.
#'
#' @return Character vector containing a Quarto code chunk.
#'
#' @keywords internal
renderAnalysisRecipeCodeChunk = function(label, codeLines) {
  c(
    paste0("```{r ", label, "}"),
    codeLines,
    "```"
  )
}

#' Encode a character scalar as R source
#'
#' @param value Character scalar.
#'
#' @return A quoted R string literal.
#'
#' @keywords internal
encodeAnalysisRecipeString = function(value) {
  encodeString(value %||% "", quote = "\"")
}
