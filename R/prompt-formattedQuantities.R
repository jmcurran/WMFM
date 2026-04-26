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

  if (is.null(ciOut) || is.null(ciOut$table)) {
    return("")
  }

  quantityTable = ciOut$table

  if (!is.data.frame(quantityTable) || nrow(quantityTable) == 0) {
    return("")
  }

  quantityTable = maybeConvertInterceptOnlyPromptQuantityTable(
    quantityTable = quantityTable,
    model = model,
    mf = mf,
    predictorNames = predictorNames
  )

  if (!identical(ciOut$mode, "derived") && length(predictorNames) > 0) {
    return("")
  }

  quantityTable = quantityTable[quantityTable$ciSection %in% c("baseline", "effect", "contrast"), , drop = FALSE]
  quantityTable = filterFormattedPromptQuantityTable(
    quantityTable = quantityTable,
    model = model
  )

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


maybeConvertInterceptOnlyPromptQuantityTable = function(
    quantityTable,
    model,
    mf,
    predictorNames) {

  if (length(predictorNames) > 0 || !is.data.frame(quantityTable) || nrow(quantityTable) == 0) {
    return(quantityTable)
  }

  interceptRows = quantityTable[quantityTable$quantity %in% "(Intercept)", , drop = FALSE]

  if (nrow(interceptRows) != 1) {
    return(quantityTable)
  }

  responseName = names(mf)[[1]]
  out = interceptRows
  out$ciSection = "baseline"
  out$quantity = buildInterceptOnlyPromptQuantityLabel(
    model = model,
    responseName = responseName
  )
  out$scale = buildInterceptOnlyPromptQuantityScale(model = model)

  if (isSupportedLogisticModel(model = model)) {
    out$estimate = stats::family(model)$linkinv(interceptRows$estimate)
    out$lower = stats::family(model)$linkinv(interceptRows$lower)
    out$upper = stats::family(model)$linkinv(interceptRows$upper)
  } else if (isSupportedPoissonModel(model = model)) {
    out$estimate = exp(interceptRows$estimate)
    out$lower = exp(interceptRows$lower)
    out$upper = exp(interceptRows$upper)
  }

  out
}

buildInterceptOnlyPromptQuantityLabel = function(model, responseName) {

  if (isSupportedLogisticModel(model = model)) {
    return(formatBinomialProbabilityLabel(model = model, outcome = "success"))
  }

  if (isSupportedPoissonModel(model = model)) {
    return(paste0("Expected ", responseName))
  }

  paste0("Mean ", responseName)
}

buildInterceptOnlyPromptQuantityScale = function(model) {

  if (isSupportedLogisticModel(model = model)) {
    return("probability")
  }

  if (isSupportedPoissonModel(model = model)) {
    return("expected value")
  }

  "response"
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
  scaleText = paste(c(scale, displayScale, primaryScale), collapse = " ")

  if (grepl("multiplier", scaleText, fixed = TRUE) || grepl("ratio", scaleText, fixed = TRUE)) {
    return("multiplier")
  }

  if (grepl("probability", scaleText, fixed = TRUE)) {
    return("probability")
  }

  if (grepl("odds", scaleText, fixed = TRUE)) {
    return("odds")
  }

  "number"
}

filterFormattedPromptQuantityTable = function(quantityTable, model) {

  if (!is.data.frame(quantityTable) || nrow(quantityTable) == 0) {
    return(quantityTable)
  }

  if (!inherits(model, "glm") ||
      !identical(model$family$family, "binomial") ||
      !identical(model$family$link, "logit")) {
    return(quantityTable)
  }

  scaleText = tolower(as.character(quantityTable$scale %||% ""))
  isBaseline = quantityTable$ciSection %in% "baseline"
  isEffectOrContrast = quantityTable$ciSection %in% c("effect", "contrast")
  isProbability = grepl("probability", scaleText, fixed = TRUE)
  isOddsMultiplier = grepl("odds multiplier", scaleText, fixed = TRUE) |
    grepl("odds ratio", scaleText, fixed = TRUE)

  keep = (!isBaseline & !isEffectOrContrast) |
    (isBaseline & isProbability) |
    (isEffectOrContrast & isOddsMultiplier)

  quantityTable[keep, , drop = FALSE]
}
