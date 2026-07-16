#' Add a completed prediction to an analysis recipe
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#' @param question Character scalar containing the user's prediction question.
#' @param newData Named list or one-row data frame of predictor settings.
#' @param target Prediction target: `mean_response` or `individual_outcome`.
#' @param level Confidence level used for the interval.
#'
#' @return An updated `wmfmAnalysisRecipe` object.
#'
#' @keywords internal
#' @noRd
addAnalysisRecipePrediction = function(
  recipe,
  question,
  newData,
  target = c("mean_response", "individual_outcome"),
  level = 0.95
) {
  validateAnalysisRecipe(recipe)
  target = match.arg(target)

  if (is.data.frame(newData)) {
    if (nrow(newData) != 1) {
      stop("Prediction recipe data must contain exactly one row.", call. = FALSE)
    }
    newData = as.list(newData[1, , drop = FALSE])
  }
  if (!is.list(newData) || is.null(names(newData)) || any(!nzchar(names(newData)))) {
    stop("Prediction recipe data must be a named list or one-row data frame.", call. = FALSE)
  }
  if (length(level) != 1 || is.na(level) || level <= 0 || level >= 1) {
    stop("Prediction confidence level must lie between 0 and 1.", call. = FALSE)
  }

  entry = list(
    question = as.character(question %||% ""),
    newData = newData,
    target = target,
    level = as.numeric(level)
  )
  predictions = recipe$sections$predictions %||% list()
  predictions[[length(predictions) + 1L]] = entry
  updateAnalysisRecipeSection(recipe, "predictions", predictions)
}

#' Add a completed contrast to an analysis recipe
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#' @param label Character description of the contrast.
#' @param codeLines Deterministically generated R source lines reproducing the contrast.
#'
#' @return An updated `wmfmAnalysisRecipe` object.
#'
#' @keywords internal
#' @noRd
addAnalysisRecipeContrast = function(recipe, label, codeLines) {
  validateAnalysisRecipe(recipe)
  if (!is.character(codeLines) || length(codeLines) == 0 || anyNA(codeLines)) {
    stop("Contrast recipe code must be a non-empty character vector.", call. = FALSE)
  }

  entry = list(
    label = as.character(label %||% "Contrast"),
    codeLines = codeLines
  )
  contrasts = recipe$sections$contrasts %||% list()
  contrasts[[length(contrasts) + 1L]] = entry
  updateAnalysisRecipeSection(recipe, "contrasts", contrasts)
}

#' Render dynamic prediction and contrast sections
#'
#' @param recipe A `wmfmAnalysisRecipe` object.
#'
#' @return Character vector containing Quarto source lines.
#'
#' @keywords internal
#' @noRd
renderAnalysisRecipeDynamicQuarto = function(recipe) {
  validateAnalysisRecipe(recipe)
  lines = character(0)

  predictions = recipe$sections$predictions %||% list()
  if (length(predictions) > 0) {
    lines = c(lines, "", "# Predictions", "")
    for (index in seq_along(predictions)) {
      lines = c(lines, renderAnalysisRecipePredictionChunk(recipe, predictions[[index]], index), "")
    }
  }

  contrasts = recipe$sections$contrasts %||% list()
  if (length(contrasts) > 0) {
    lines = c(lines, "", "# Contrasts", "")
    for (index in seq_along(contrasts)) {
      lines = c(lines, renderAnalysisRecipeContrastChunk(contrasts[[index]], index), "")
    }
  }

  lines
}

#' Render one prediction chunk
#'
#' @keywords internal
#' @noRd
renderAnalysisRecipePredictionChunk = function(recipe, prediction, index) {
  newDataLines = renderAnalysisRecipeNamedListAsDataFrame(prediction$newData)
  level = format(prediction$level, scientific = FALSE, trim = TRUE)
  modelType = recipe$model$modelType %||% ""
  target = prediction$target %||% "mean_response"

  if (identical(modelType, "lm")) {
    interval = if (identical(target, "individual_outcome")) "prediction" else "confidence"
    predictionLines = c(
      "predictionResult = predict(",
      "  modelFit,",
      "  newdata = predictionData,",
      paste0("  interval = ", encodeAnalysisRecipeString(interval), ","),
      paste0("  level = ", level),
      ")",
      "predictionResult"
    )
  } else {
    predictionLines = c(
      "predictionResult = predict(",
      "  modelFit,",
      "  newdata = predictionData,",
      "  type = \"response\",",
      "  se.fit = TRUE",
      ")",
      "predictionResult"
    )
  }

  renderAnalysisRecipeCodeChunk(
    paste0("prediction-", index),
    c(newDataLines, predictionLines)
  )
}

#' Render one contrast chunk
#'
#' @keywords internal
#' @noRd
renderAnalysisRecipeContrastChunk = function(contrast, index) {
  renderAnalysisRecipeCodeChunk(
    paste0("contrast-", index),
    contrast$codeLines
  )
}

#' Render named values as a one-row data frame
#'
#' @keywords internal
#' @noRd
renderAnalysisRecipeNamedListAsDataFrame = function(values) {
  valueLines = vapply(
    seq_along(values),
    function(index) {
      suffix = if (index < length(values)) "," else ""
      paste0("  ", names(values)[index], " = ", deparse(values[[index]])[1], suffix)
    },
    character(1)
  )
  c("predictionData = data.frame(", valueLines, ")")
}
