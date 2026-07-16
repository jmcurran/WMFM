#' Create an internal WMFM analysis recipe
#'
#' Creates the deterministic analysis-state object used by later renderers.
#' The recipe stores structured inputs and settings rather than rendered code
#' or copied console output.
#'
#' @param data Structured data-source metadata.
#' @param preparation Structured data-preparation metadata.
#' @param model Structured fitted-model metadata.
#' @param sections Optional structured post-fit analysis sections.
#' @param output Structured output preferences.
#' @param schemaVersion Integer recipe schema version.
#'
#' @return An object of class `wmfmAnalysisRecipe`.
#'
#' @keywords internal
newAnalysisRecipe = function(
  data,
  preparation,
  model,
  sections = list(),
  output = list(),
  schemaVersion = 1L
) {
  recipe = list(
    schemaVersion = as.integer(schemaVersion),
    data = data,
    preparation = preparation,
    model = model,
    sections = utils::modifyList(
      list(
        summary = list(enabled = TRUE),
        anova = list(enabled = TRUE),
        confidenceIntervals = list(enabled = TRUE, level = 0.95),
        diagnostics = list(enabled = TRUE),
        modelPlot = list(
          enabled = TRUE,
          plotType = "observedFitted",
          showSmoothTrend = TRUE
        ),
        predictions = list(),
        contrasts = list()
      ),
      sections
    ),
    output = output
  )

  class(recipe) = c("wmfmAnalysisRecipe", "list")
  validateAnalysisRecipe(recipe)
  recipe
}

#' Test whether an object is a WMFM analysis recipe
#'
#' @param x Object to test.
#'
#' @return `TRUE` when `x` is a valid classed analysis recipe.
#'
#' @keywords internal
isAnalysisRecipe = function(x) {
  inherits(x, "wmfmAnalysisRecipe")
}

#' Validate a WMFM analysis recipe
#'
#' @param recipe Object to validate.
#'
#' @return The validated recipe, invisibly.
#'
#' @keywords internal
validateAnalysisRecipe = function(recipe) {
  if (!isAnalysisRecipe(recipe)) {
    stop("The object is not a WMFM analysis recipe.", call. = FALSE)
  }

  requiredNames = c(
    "schemaVersion",
    "data",
    "preparation",
    "model",
    "sections",
    "output"
  )

  missingNames = setdiff(requiredNames, names(recipe))
  if (length(missingNames) > 0) {
    stop(
      paste0(
        "The analysis recipe is missing required field(s): ",
        paste(missingNames, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (length(recipe$schemaVersion) != 1 || is.na(recipe$schemaVersion) || recipe$schemaVersion < 1) {
    stop("The analysis recipe schema version must be a positive integer.", call. = FALSE)
  }

  if (!is.list(recipe$data) || !is.list(recipe$preparation) ||
      !is.list(recipe$model) || !is.list(recipe$sections) ||
      !is.list(recipe$output)) {
    stop("Analysis recipe components must be stored as lists.", call. = FALSE)
  }

  requiredModelNames = c("formula", "modelType", "response", "predictors")
  missingModelNames = setdiff(requiredModelNames, names(recipe$model))
  if (length(missingModelNames) > 0) {
    stop(
      paste0(
        "The analysis recipe model metadata is missing field(s): ",
        paste(missingModelNames, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(recipe)
}

#' Replace one analysis recipe section
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#' @param sectionName Character scalar naming the section.
#' @param section Structured replacement section.
#'
#' @return An updated `wmfmAnalysisRecipe` object.
#'
#' @keywords internal
updateAnalysisRecipeSection = function(recipe, sectionName, section) {
  validateAnalysisRecipe(recipe)

  if (length(sectionName) != 1 || is.na(sectionName) || !nzchar(trimws(sectionName))) {
    stop("The analysis recipe section name must be a non-empty character scalar.", call. = FALSE)
  }

  if (!is.list(section)) {
    stop("Analysis recipe sections must be stored as lists.", call. = FALSE)
  }

  recipe$sections[[sectionName]] = section
  validateAnalysisRecipe(recipe)
  recipe
}
