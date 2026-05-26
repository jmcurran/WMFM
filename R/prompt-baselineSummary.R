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

  hasFactorPredictor = any(vapply(mf[predictorNames], is.factor, logical(1)))
  hasNumericPredictor = any(vapply(mf[predictorNames], is.numeric, logical(1)))

  lines = c(
    "Precomputed anchored baseline fitted values:",
    "Use these values directly when describing baseline fitted values or group comparisons at the chosen numeric anchor.",
    "Do not derive baseline fitted values from the intercept alone."
  )

  if (isTRUE(hasFactorPredictor) && isTRUE(hasNumericPredictor) && nrow(baselineRows) > 1) {
    lines = c(
      lines,
      "Mandatory anchored factor comparison:",
      "- When the research question asks about a factor predictor and a numeric predictor, include one concise anchored comparison for the factor predictor.",
      "- State the numeric anchor used for the comparison, then report the reference fitted value, the comparison fitted value, and the approximate difference on the response scale.",
      "- Do not omit the factor comparison merely because the numeric slope is also reported."
    )
  }

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


#' Ensure anchored factor comparisons appear in explanation text
#'
#' Adds a deterministic anchored factor-comparison sentence when a fitted linear
#' model contains both a factor predictor and a numeric predictor, but the LLM
#' explanation omitted the anchored fitted values needed for a student-facing
#' interpretation.
#'
#' @param text Character scalar explanation text.
#' @param model A fitted model object.
#'
#' @return A character scalar.
#' @keywords internal
ensureAnchoredFactorComparisonText = function(text, model) {

  if (!is.character(text) || length(text) != 1 || !nzchar(text)) {
    return(text)
  }

  sentence = buildAnchoredFactorComparisonSentence(model)

  if (!nzchar(sentence)) {
    return(text)
  }

  if (anchoredFactorComparisonAlreadyPresent(text = text, sentence = sentence)) {
    return(text)
  }

  insertAnchoredFactorComparisonSentence(text = text, sentence = sentence)
}

buildAnchoredFactorComparisonSentence = function(model) {

  if (!inherits(model, "lm") || inherits(model, "glm")) {
    return("")
  }

  mf = tryCatch(stats::model.frame(model), error = function(e) NULL)

  if (is.null(mf) || ncol(mf) < 3) {
    return("")
  }

  responseName = names(mf)[[1]]
  predictorNames = names(mf)[-1]
  factorPredictors = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]
  numericPredictors = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(factorPredictors) == 0 || length(numericPredictors) == 0) {
    return("")
  }

  factorName = factorPredictors[[1]]
  numericName = numericPredictors[[1]]
  factorLevels = levels(mf[[factorName]])

  if (length(factorLevels) < 2) {
    return("")
  }

  anchorValue = chooseAnchoredFactorComparisonNumericAnchor(mf[[numericName]])

  if (!is.finite(anchorValue)) {
    return("")
  }

  newData = buildAnchoredFactorComparisonNewData(
    mf = mf,
    predictorNames = predictorNames,
    factorName = factorName,
    factorLevels = factorLevels[seq_len(2)],
    numericName = numericName,
    anchorValue = anchorValue
  )

  prediction = tryCatch(
    stats::predict(model, newdata = newData, interval = "confidence", level = 0.95),
    error = function(e) NULL
  )

  if (is.null(prediction) || nrow(as.data.frame(prediction)) < 2) {
    return("")
  }

  prediction = as.data.frame(prediction)
  estimateOne = prediction$fit[[1]]
  estimateTwo = prediction$fit[[2]]
  difference = estimateTwo - estimateOne

  if (!is.finite(estimateOne) || !is.finite(estimateTwo) || !is.finite(difference)) {
    return("")
  }

  paste0(
    "At ", numericName, " = ",
    formatExplanationQuantity(anchorValue, quantityType = "anchor"),
    ", the expected ", responseName, " is about ",
    formatExplanationQuantity(estimateOne, quantityType = "number"),
    " for ", factorName, " = ", factorLevels[[1]],
    " and about ",
    formatExplanationQuantity(estimateTwo, quantityType = "number"),
    " for ", factorName, " = ", factorLevels[[2]],
    ", a difference of about ",
    formatExplanationQuantity(abs(difference), quantityType = "number"),
    " points associated with ", factorName, " = ", factorLevels[[2]], "."
  )
}

chooseAnchoredFactorComparisonNumericAnchor = function(x) {

  x = x[is.finite(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  if (min(x) <= 0 && max(x) >= 0) {
    return(0)
  }

  mean(x)
}

buildAnchoredFactorComparisonNewData = function(
    mf,
    predictorNames,
    factorName,
    factorLevels,
    numericName,
    anchorValue
) {

  baseRow = as.data.frame(lapply(predictorNames, function(predictorName) {
    column = mf[[predictorName]]

    if (is.factor(column)) {
      factor(levels(column)[[1]], levels = levels(column))
    } else if (is.numeric(column)) {
      chooseAnchoredFactorComparisonNumericAnchor(column)
    } else {
      column[[which(!is.na(column))[1]]]
    }
  }), stringsAsFactors = FALSE)

  names(baseRow) = predictorNames
  baseRow[[numericName]] = anchorValue
  out = baseRow[rep(1, length(factorLevels)), , drop = FALSE]
  out[[factorName]] = factor(factorLevels, levels = levels(mf[[factorName]]))
  row.names(out) = NULL
  out
}

anchoredFactorComparisonAlreadyPresent = function(text, sentence) {

  sentenceNumbers = regmatches(sentence, gregexpr("-?[0-9]+(?:\\.[0-9]+)?", sentence, perl = TRUE))[[1]]
  sentenceNumbers = unique(sentenceNumbers[nzchar(sentenceNumbers)])

  if (length(sentenceNumbers) < 3) {
    return(FALSE)
  }

  requiredNumbers = sentenceNumbers[seq_len(min(3, length(sentenceNumbers)))]
  all(vapply(requiredNumbers, grepl, logical(1), x = text, fixed = TRUE))
}

insertAnchoredFactorComparisonSentence = function(text, sentence) {

  paragraphBreak = "\n\n"
  paragraphs = strsplit(text, paragraphBreak, fixed = TRUE)[[1]]

  followupIndex = grep("^For the follow-up question", paragraphs)

  if (length(followupIndex) > 0) {
    insertAt = max(1, followupIndex[[1]] - 1)
    paragraphs = append(paragraphs, sentence, after = insertAt)
    return(paste(paragraphs, collapse = paragraphBreak))
  }

  paste(text, sentence, sep = paragraphBreak)
}
