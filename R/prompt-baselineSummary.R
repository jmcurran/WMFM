#' Build anchored baseline fitted-value context for explanation prompts
#'
#' Uses the confidence-interval teaching helpers to compute baseline fitted
#' values at the chosen numeric anchor, so the explanation prompt does not rely
#' on the language model to derive anchored counts, odds, or means from the
#' intercept alone.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#' @param predictorNames Optional predictor names.
#'
#' @return A character scalar, or an empty string if no suitable anchored
#'   baseline summary is available.
#' @keywords internal
buildAnchoredBaselinePromptBlock = function(
    model,
    mf = NULL,
    predictorNames = NULL
) {

  if (is.null(mf)) {
    mf = model.frame(model)
  }

  if (is.null(predictorNames)) {
    predictorNames = names(mf)[-1]
  }

  if (length(predictorNames) == 0) {
    return("")
  }

  numericReference = chooseModelNumericReference(
    model = model,
    mf = mf,
    predictorNames = predictorNames
  )

  ciOut = tryCatch(
    buildModelConfidenceIntervalData(
      model = model,
      numericReference = numericReference
    ),
    error = function(e) {
      NULL
    }
  )

  if (is.null(ciOut) || !identical(ciOut$mode, "derived") || is.null(ciOut$table)) {
    return("")
  }

  baselineRows = ciOut$table[ciOut$table$ciSection == "baseline", , drop = FALSE]

  if (nrow(baselineRows) == 0) {
    return("")
  }

  if (inherits(model, "glm") && identical(model$family$family, "poisson")) {
    baselineRows = baselineRows[baselineRows$scale == "expected value", , drop = FALSE]
  } else if (inherits(model, "glm") && identical(model$family$family, "binomial")) {
    baselineRows = baselineRows[baselineRows$scale == "probability", , drop = FALSE]
  }

  if (nrow(baselineRows) == 0) {
    return("")
  }

  detailSettings = list()

  if (!is.null(ciOut$details) && length(ciOut$details) > 0) {
    for (detail in ciOut$details) {
      if (!is.null(detail$label) && nzchar(detail$label)) {
        detailSettings[[detail$label]] = detail$settings
      }
    }
  }

  lines = c(
    "Precomputed anchored baseline fitted values:",
    "Use these values directly when describing baseline fitted values or group comparisons at the chosen numeric anchor.",
    "Do not derive baseline fitted values from the intercept alone."
  )

  for (i in seq_len(nrow(baselineRows))) {
    label = baselineRows$quantity[[i]]
    quantityType = getAnchoredBaselinePromptQuantityType(baselineRows[i, , drop = FALSE])
    estimateText = formatExplanationQuantity(baselineRows$estimate[[i]], quantityType = quantityType)
    lowerText = formatExplanationQuantity(baselineRows$lower[[i]], quantityType = quantityType)
    upperText = formatExplanationQuantity(baselineRows$upper[[i]], quantityType = quantityType)

    line = paste0(
      "- ",
      label,
      ": estimate = ",
      estimateText,
      " (95% confidence interval ",
      lowerText,
      " to ",
      upperText,
      ")"
    )

    settings = detailSettings[[label]]

    if (!is.null(settings) && nzchar(settings)) {
      line = paste0(line, "; settings: ", settings)
    }

    lines = c(lines, line)
  }

  paste(lines, collapse = "\n")
}

getAnchoredBaselinePromptQuantityType = function(row) {

  scale = tolower(as.character(row$scale[[1]]))

  if (grepl("probability", scale, fixed = TRUE)) {
    return("probability")
  }

  if (grepl("odds", scale, fixed = TRUE)) {
    return("odds")
  }

  "number"
}
