#' Build formatted explanation quantities for prompt input
#'
#' Builds a student-ready prompt block from deterministic confidence-interval
#' evidence. Raw coefficients and raw confidence-interval matrices should stay
#' in audit/developer metadata; this block is for the language model prompt.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#' @param predictorNames Optional predictor names.
#'
#' @return A character scalar containing formatted prompt quantities, or an
#'   empty string if no deterministic quantity table can be built.
#' @keywords internal
buildFormattedPromptQuantityBlock = function(
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

  quantityTable = ciOut$table

  if (!is.data.frame(quantityTable) || nrow(quantityTable) == 0) {
    return("")
  }

  quantityTable = quantityTable[quantityTable$ciSection %in% c("baseline", "effect", "contrast"), , drop = FALSE]

  if (nrow(quantityTable) == 0) {
    return("")
  }

  detailSettings = buildFormattedPromptQuantitySettings(ciOut$details)

  lines = c(
    "Formatted model quantities for the explanation:",
    "Use these formatted quantities directly. Do not round, exponentiate, back-transform, or recompute them.",
    "Keep each estimate and its 95% confidence interval together in the same sentence where possible."
  )

  for (sectionName in c("baseline", "effect", "contrast")) {
    sectionRows = quantityTable[quantityTable$ciSection %in% sectionName, , drop = FALSE]

    if (nrow(sectionRows) > 0) {
      lines = c(lines, buildFormattedPromptQuantitySection(sectionName, sectionRows, detailSettings))
    }
  }

  paste(lines, collapse = "\n")
}

buildFormattedPromptQuantitySettings = function(details) {

  out = list()

  if (is.null(details) || length(details) == 0) {
    return(out)
  }

  for (detail in details) {
    if (!is.null(detail$label) && nzchar(detail$label)) {
      out[[detail$label]] = detail$settings
    }
  }

  out
}

buildFormattedPromptQuantitySection = function(sectionName, sectionRows, detailSettings) {

  sectionTitle = switch(
    sectionName,
    baseline = "Baseline or fitted values:",
    effect = "Effects and comparisons:",
    contrast = "Effects and comparisons:",
    "Model quantities:"
  )

  lines = sectionTitle

  for (i in seq_len(nrow(sectionRows))) {
    lines = c(lines, buildFormattedPromptQuantityLine(sectionRows[i, , drop = FALSE], detailSettings))
  }

  lines
}

buildFormattedPromptQuantityLine = function(row, detailSettings) {

  label = as.character(row$quantity[[1]])
  scale = as.character(row$scale[[1]])
  estimate = row$estimate[[1]]
  lower = row$lower[[1]]
  upper = row$upper[[1]]

  quantityType = getFormattedPromptQuantityType(row)
  estimateText = formatExplanationQuantity(estimate, quantityType = quantityType)
  lowerText = formatExplanationQuantity(lower, quantityType = quantityType)
  upperText = formatExplanationQuantity(upper, quantityType = quantityType)

  line = paste0(
    "- ",
    label,
    " on the ",
    scale,
    " scale: estimate = ",
    estimateText,
    "; 95% confidence interval = ",
    lowerText,
    " to ",
    upperText
  )

  settings = detailSettings[[label]]

  if (!is.null(settings) && nzchar(settings)) {
    line = paste0(line, "; settings: ", settings)
  }

  line
}

getFormattedPromptQuantityType = function(row) {

  scale = tolower(as.character(row$scale[[1]]))
  displayScale = tolower(as.character(row$displayScale[[1]] %||% ""))
  primaryScale = tolower(as.character(row$primaryScale[[1]] %||% ""))
  scaleText = paste(scale, displayScale, primaryScale, collapse = " ")

  if (grepl("probability", scaleText, fixed = TRUE)) {
    return("probability")
  }

  if (grepl("odds", scaleText, fixed = TRUE)) {
    return("odds")
  }

  if (grepl("multiplier", scaleText, fixed = TRUE) || grepl("ratio", scaleText, fixed = TRUE)) {
    return("multiplier")
  }

  "number"
}
