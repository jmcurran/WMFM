#' Build interpretable confidence-interval output for a fitted model
#'
#' Creates a compact table of confidence intervals for quantities that are
#' easier for students to interpret than a raw coefficient table. For supported
#' models, the helper prefers derived teaching rows such as fitted quantities at
#' simple reference settings and one-unit covariate effects on the most natural
#' teaching scale. When the model structure is too complex for a stable derived
#' summary, the helper falls back to a coefficient table.
#'
#' Derived mode currently supports:
#' - models with no interaction terms
#' - models with exactly one factor predictor, one numeric predictor, and one
#'   interaction term between them
#'
#' All other interaction structures fall back to coefficient mode.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param level Confidence level. Defaults to `0.95`.
#' @param numericReference How numeric predictors should be fixed when building
#'   fitted-quantity rows. One of `"mean"` or `"zero"`. Defaults to `"mean"`.
#'   The app may override this with a data-aware helper so that fitted-value
#'   summaries are anchored at sample means when 0 lies outside the observed
#'   numeric range.
#'
#' @return A list with components:
#' \describe{
#'   \item{table}{A data frame ready for display in the confidence-interval tab.}
#'   \item{details}{A list of labelled derivation records for drill-down display.}
#'   \item{note}{Optional explanatory note for the UI.}
#'   \item{teachingNote}{Optional short teaching note about variance and covariance.}
#'   \item{vcovTable}{The variance-covariance matrix rounded for display.}
#'   \item{mode}{Either `"derived"` or `"coefficient"`.}
#' }
#'
#' @keywords internal
#'
#' @importFrom stats coef delete.response family model.frame model.matrix
#' @importFrom stats predict qnorm qt terms vcov
#' @importFrom utils combn
buildModelConfidenceIntervalData = function(
    model,
    level = 0.95,
    numericReference = c("mean", "zero")
) {

  numericReference = match.arg(numericReference)

  if (!is.numeric(level) || length(level) != 1 || is.na(level) || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.", call. = FALSE)
  }

  mf = model.frame(model)
  predictorNames = names(mf)[-1]
  vcovTable = round(vcov(model), 3)
  teachingNote = buildModelConfidenceIntervalTeachingNote(model = model)

  if (length(predictorNames) == 0) {
    out = buildCoefficientOnlyConfidenceIntervalData(model = model, level = level)
    out$note = "This model has no predictors, so the confidence-interval table shows the intercept only."
    out$teachingNote = teachingNote
    out$vcovTable = vcovTable
    return(out)
  }

  derivedPlan = classifyModelConfidenceIntervalPlan(model = model, mf = mf)

  if (identical(derivedPlan$mode, "coefficient")) {
    out = buildCoefficientOnlyConfidenceIntervalData(model = model, level = level)
    out$note = derivedPlan$note
    out$teachingNote = teachingNote
    out$vcovTable = vcovTable
    return(out)
  }

  derivedOut = buildDerivedConfidenceIntervalData(
    model = model,
    mf = mf,
    level = level,
    numericReference = numericReference,
    derivedPlan = derivedPlan
  )

  derivedOut$teachingNote = teachingNote
  derivedOut$vcovTable = vcovTable
  derivedOut
}

#' Classify CI output mode for a fitted model
#'
#' @param model A fitted model object.
#' @param mf Model frame for the fitted model.
#'
#' @return A list describing the chosen mode.
#' @keywords internal
classifyModelConfidenceIntervalPlan = function(model, mf) {

  tt = terms(model)
  termLabels = attr(tt, "term.labels")

  if (is.null(termLabels)) {
    termLabels = character(0)
  }

  predictorNames = names(mf)[-1]
  interactionTerms = termLabels[grepl(":", termLabels, fixed = TRUE)]
  hasInteractions = length(interactionTerms) > 0

  if (!hasInteractions) {
    return(list(
      mode = "derived",
      interactionType = "none",
      predictorNames = predictorNames,
      factorName = NULL,
      numericName = NULL,
      interactionTerm = NULL,
      note = buildDerivedModeNote(mf = mf, numericReference = "context-dependent")
    ))
  }

  supportedInteractionFamily = (
    isSupportedLogisticModel(model = model) ||
      isSupportedPoissonModel(model = model)
  )

  if (!supportedInteractionFamily) {
    return(list(
      mode = "coefficient",
      note = paste(
        "This model contains interaction terms, so the confidence-interval table is shown on the coefficient scale.",
        "Derived interaction teaching rows are only supported for logistic GLMs and Poisson GLMs",
        "in the simple one-factor-plus-one-numeric interaction case."
      )
    ))
  }

  factorNames = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]
  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(factorNames) != 1 || length(numericNames) != 1 || length(interactionTerms) != 1) {
    return(list(
      mode = "coefficient",
      note = paste(
        "This model contains interactions beyond the supported teaching summary case.",
        "Derived rows are only used when there is exactly one factor predictor,",
        "exactly one numeric predictor, and exactly one interaction term."
      )
    ))
  }

  factorName = factorNames[[1]]
  numericName = numericNames[[1]]
  expectedInteractionTerms = c(
    paste0(factorName, ":", numericName),
    paste0(numericName, ":", factorName)
  )

  if (!(interactionTerms[[1]] %in% expectedInteractionTerms)) {
    return(list(
      mode = "coefficient",
      note = paste(
        "This interaction structure is shown on the coefficient scale because",
        "the single interaction term is not the supported factor-by-numeric pattern."
      )
    ))
  }

  list(
    mode = "derived",
    interactionType = "simple",
    predictorNames = predictorNames,
    factorName = factorName,
    numericName = numericName,
    interactionTerm = interactionTerms[[1]],
    note = paste(
      "Rows are tagged internally as fitted quantities and predictor effects.",
      "For the interaction, fitted quantities are shown at the reference value of",
      numericName,
      "and slope rows are shown separately within each level of",
      factorName,
      "."
    )
  )
}

#' Build derived CI output for supported model structures
#'
#' @param model A fitted model object.
#' @param mf Model frame.
#' @param level Confidence level.
#' @param numericReference Numeric reference choice.
#' @param derivedPlan Plan from \code{classifyModelConfidenceIntervalPlan()}.
#'
#' @return A list in the same format as \code{buildModelConfidenceIntervalData()}.
#' @keywords internal
buildDerivedConfidenceIntervalData = function(
    model,
    mf,
    level,
    numericReference,
    derivedPlan
) {

  familyObj = if (inherits(model, "glm")) {
    family(model)
  } else {
    NULL
  }

  baseInfo = buildConfidenceIntervalBaseInfo(
    mf = mf,
    numericReference = numericReference,
    predictorNames = derivedPlan$predictorNames
  )

  rows = list()
  details = list()

  appendRow = function(
      quantity,
      estimate,
      lower,
      upper,
      scale,
      section,
      settings,
      weights,
      scaleNote
  ) {

    rows[[length(rows) + 1]] <<- data.frame(
      ciSection = section,
      quantity = quantity,
      estimate = round(estimate, 3),
      lower = round(lower, 3),
      upper = round(upper, 3),
      scale = scale,
      stringsAsFactors = FALSE
    )

    details[[length(details) + 1]] <<- list(
      label = quantity,
      quantity = quantity,
      section = section,
      settings = settings,
      builtFrom = buildLinearCombinationText(weights),
      varianceFormula = buildLinearCombinationVarianceText(weights),
      scaleNote = scaleNote
    )
  }

  if (identical(derivedPlan$interactionType, "simple")) {
    buildSimpleInteractionConfidenceIntervalRows(
      model = model,
      mf = mf,
      familyObj = familyObj,
      level = level,
      baseInfo = baseInfo,
      factorName = derivedPlan$factorName,
      numericName = derivedPlan$numericName,
      appendRow = appendRow
    )
  } else {
    buildNoInteractionConfidenceIntervalRows(
      model = model,
      mf = mf,
      familyObj = familyObj,
      level = level,
      baseInfo = baseInfo,
      appendRow = appendRow
    )
  }

  ciTable = do.call(rbind, rows)
  ciTable = enrichConfidenceIntervalDisplayTable(
    model = model,
    ciTable = ciTable
  )

  list(
    table = ciTable,
    details = details,
    note = buildDerivedModeNote(mf = mf, numericReference = numericReference),
    teachingNote = NULL,
    vcovTable = NULL,
    mode = "derived"
  )
}

#' Build CI rows for models without interactions
#'
#' @param model A fitted model object.
#' @param mf Model frame.
#' @param familyObj Optional GLM family object.
#' @param level Confidence level.
#' @param baseInfo Base-setting information.
#' @param appendRow Row appender closure.
#'
#' @return Invisibly returns \code{NULL}.
#' @keywords internal
buildNoInteractionConfidenceIntervalRows = function(
    model,
    mf,
    familyObj,
    level,
    baseInfo,
    appendRow
) {

  predictorNames = names(mf)[-1]
  responseName = names(mf)[1]
  factorNames = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]
  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(factorNames) == 0) {
    addFittedQuantityRows(
      model = model,
      mf = mf,
      familyObj = familyObj,
      level = level,
      newData = baseInfo$baseRow,
      labelContext = list(type = "baseline", responseName = responseName),
      settings = baseInfo$baselineSettings,
      numericReference = baseInfo$numericReference,
      appendRow = appendRow
    )
  }

  for (factorName in factorNames) {
    levs = levels(mf[[factorName]])

    for (lvl in levs) {
      newData = baseInfo$baseRow
      newData[[factorName]] = factor(lvl, levels = levs)

      settings = paste0(
        factorName,
        " = ",
        lvl,
        ". ",
        buildOtherBaseSettingsText(
          mf = mf,
          baseRow = baseInfo$baseRow,
          excludeVarName = factorName,
          numericReference = baseInfo$numericReference
        )
      )

      addFittedQuantityRows(
        model = model,
        mf = mf,
        familyObj = familyObj,
        level = level,
        newData = newData,
        labelContext = list(type = "factorLevel", factorName = factorName, level = lvl),
        settings = settings,
        numericReference = baseInfo$numericReference,
        appendRow = appendRow
      )
    }
  }

  for (numericName in numericNames) {
    addNumericEffectRow(
      model = model,
      mf = mf,
      familyObj = familyObj,
      level = level,
      weights = setNames(1, numericName),
      numericName = numericName,
      factorLevelText = NULL,
      appendRow = appendRow
    )
  }

  invisible(NULL)
}

#' Build CI rows for supported simple interactions
#'
#' @param model A fitted model object.
#' @param mf Model frame.
#' @param familyObj Optional GLM family object.
#' @param level Confidence level.
#' @param baseInfo Base-setting information.
#' @param factorName Name of the factor predictor.
#' @param numericName Name of the numeric predictor.
#' @param appendRow Row appender closure.
#'
#' @return Invisibly returns \code{NULL}.
#' @keywords internal
buildSimpleInteractionConfidenceIntervalRows = function(
    model,
    mf,
    familyObj,
    level,
    baseInfo,
    factorName,
    numericName,
    appendRow
) {

  levs = levels(mf[[factorName]])

  for (lvl in levs) {
    newData = baseInfo$baseRow
    newData[[factorName]] = factor(lvl, levels = levs)

    settings = paste0(
      factorName,
      " = ",
      lvl,
      "; ",
      numericName,
      " = ",
      formatConfidenceIntervalNumber(baseInfo$baseRow[[numericName]][1]),
      "."
    )

    addFittedQuantityRows(
      model = model,
      mf = mf,
      familyObj = familyObj,
      level = level,
      newData = newData,
      labelContext = list(type = "factorLevel", factorName = factorName, level = lvl),
      settings = settings,
      numericReference = baseInfo$numericReference,
      appendRow = appendRow
    )
  }

  for (lvl in levs) {
    weights = buildSimpleInteractionSlopeWeights(
      model = model,
      factorName = factorName,
      factorLevel = lvl,
      numericName = numericName
    )

    addNumericEffectRow(
      model = model,
      mf = mf,
      familyObj = familyObj,
      level = level,
      weights = weights,
      numericName = numericName,
      factorLevelText = paste0(factorName, " = ", lvl),
      appendRow = appendRow
    )
  }

  invisible(NULL)
}

#' Add fitted-quantity CI rows for a single predictor setting
#'
#' @param model A fitted model object.
#' @param mf Model frame.
#' @param familyObj Optional GLM family object.
#' @param level Confidence level.
#' @param newData One-row new-data frame.
#' @param labelContext Context used to build row labels.
#' @param settings Human-readable settings text.
#' @param numericReference Numeric reference choice used when completing
#'   any omitted numeric predictors in \code{newData}.
#' @param appendRow Row appender closure.
#'
#' @return Invisibly returns \code{NULL}.
#' @keywords internal
addFittedQuantityRows = function(
    model,
    mf,
    familyObj,
    level,
    newData,
    labelContext,
    settings,
    numericReference,
    appendRow
) {

  prediction = buildConfidenceIntervalPrediction(
    model = model,
    newData = newData,
    level = level,
    numericReference = numericReference
  )

  weights = prediction$weights
  notation = buildConfidenceIntervalNotation(model = model, mf = mf)

  if (is.null(familyObj) || identical(familyObj$link, "identity")) {
    quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation)

    appendRow(
      quantity = quantity,
      estimate = prediction$eta,
      lower = prediction$lowerEta,
      upper = prediction$upperEta,
      scale = notation$responseScale,
      section = "baseline",
      settings = settings,
      weights = weights,
      scaleNote = "Computed directly on the response scale."
    )

    return(invisible(NULL))
  }

  if (isSupportedLogisticModel(model = model)) {
    successProb = familyObj$linkinv(prediction$eta)
    lowerProb = familyObj$linkinv(prediction$lowerEta)
    upperProb = familyObj$linkinv(prediction$upperEta)

    successOdds = exp(prediction$eta)
    lowerOdds = exp(prediction$lowerEta)
    upperOdds = exp(prediction$upperEta)

    appendRow(
      quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation, quantityType = "probabilitySuccess"),
      estimate = successProb,
      lower = lowerProb,
      upper = upperProb,
      scale = "probability",
      section = "baseline",
      settings = settings,
      weights = weights,
      scaleNote = paste(
        "Computed on the logit scale, then transformed to",
        notation$probabilitySuccess,
        "using the inverse logit function."
      )
    )

    appendRow(
      quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation, quantityType = "oddsSuccess"),
      estimate = successOdds,
      lower = lowerOdds,
      upper = upperOdds,
      scale = "odds",
      section = "baseline",
      settings = settings,
      weights = weights,
      scaleNote = paste(
        "Computed on the logit scale and exponentiated to the",
        notation$oddsSuccess,
        "scale."
      )
    )

    appendRow(
      quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation, quantityType = "probabilityFailure"),
      estimate = 1 - successProb,
      lower = 1 - upperProb,
      upper = 1 - lowerProb,
      scale = "probability",
      section = "baseline",
      settings = settings,
      weights = weights,
      scaleNote = paste(
        "Computed as 1 -",
        notation$probabilitySuccess,
        "after transforming the logit-scale interval."
      )
    )

    appendRow(
      quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation, quantityType = "oddsFailure"),
      estimate = exp(-prediction$eta),
      lower = exp(-prediction$upperEta),
      upper = exp(-prediction$lowerEta),
      scale = "odds",
      section = "baseline",
      settings = settings,
      weights = weights,
      scaleNote = paste(
        "Computed as the reciprocal of",
        notation$oddsSuccess,
        "."
      )
    )

    return(invisible(NULL))
  }

  if (isSupportedPoissonModel(model = model)) {
    appendRow(
      quantity = buildFittedQuantityLabel(labelContext = labelContext, notation = notation, quantityType = "mean"),
      estimate = exp(prediction$eta),
      lower = exp(prediction$lowerEta),
      upper = exp(prediction$upperEta),
      scale = "expected value",
      section = "baseline",
      settings = settings,
      weights = weights,
      scaleNote = "Computed on the log scale, then exponentiated to the E(Y) scale."
    )
  }

  invisible(NULL)
}

#' Add a numeric-effect CI row
#'
#' @param model A fitted model object.
#' @param mf Model frame.
#' @param familyObj Optional GLM family object.
#' @param level Confidence level.
#' @param weights Named coefficient weights for the linear combination.
#' @param numericName Name of the numeric predictor.
#' @param factorLevelText Optional factor-level context.
#' @param appendRow Row appender closure.
#'
#' @return Invisibly returns \code{NULL}.
#' @keywords internal
addNumericEffectRow = function(
    model,
    mf,
    familyObj,
    level,
    weights,
    numericName,
    factorLevelText,
    appendRow
) {

  interval = buildLinearCombinationInterval(
    model = model,
    weights = weights,
    level = level
  )

  notation = buildConfidenceIntervalNotation(model = model, mf = mf)
  settings = if (is.null(factorLevelText)) {
    "A 1-unit increase in the named numeric predictor, with no additional row-specific settings."
  } else {
    paste0("A 1-unit increase in ", numericName, " when ", factorLevelText, ".")
  }

  if (is.null(familyObj) || identical(familyObj$link, "identity")) {
    quantity = paste0("Change in ", names(mf)[1], " for a 1-unit increase in ", numericName)

    if (!is.null(factorLevelText)) {
      quantity = paste0(quantity, " when ", factorLevelText)
    }

    appendRow(
      quantity = quantity,
      estimate = interval$estimate,
      lower = interval$lower,
      upper = interval$upper,
      scale = notation$responseScale,
      section = "effect",
      settings = settings,
      weights = interval$weights,
      scaleNote = if (length(weights) == 1) {
        "Computed directly from a single coefficient."
      } else {
        "Computed by combining the relevant coefficients on the response scale."
      }
    )

    return(invisible(NULL))
  }

  if (isSupportedLogisticModel(model = model)) {
    quantity = paste0(notation$oddsSuccess, " multiplier for a 1-unit increase in ", numericName)

    if (!is.null(factorLevelText)) {
      quantity = paste0(quantity, " when ", factorLevelText)
    }

    appendRow(
      quantity = quantity,
      estimate = exp(interval$estimate),
      lower = exp(interval$lower),
      upper = exp(interval$upper),
      scale = "odds multiplier",
      section = "effect",
      settings = settings,
      weights = interval$weights,
      scaleNote = if (length(weights) == 1) {
        "Computed from one coefficient on the logit scale, then exponentiated to the odds-multiplier scale."
      } else {
        "Computed by combining coefficients on the logit scale, then exponentiating to the odds-multiplier scale."
      }
    )

    return(invisible(NULL))
  }

  if (isSupportedPoissonModel(model = model)) {
    quantity = paste0(notation$mean, " multiplier for a 1-unit increase in ", numericName)

    if (!is.null(factorLevelText)) {
      quantity = paste0(quantity, " when ", factorLevelText)
    }

    appendRow(
      quantity = quantity,
      estimate = exp(interval$estimate),
      lower = exp(interval$lower),
      upper = exp(interval$upper),
      scale = "E(Y) multiplier",
      section = "effect",
      settings = settings,
      weights = interval$weights,
      scaleNote = if (length(weights) == 1) {
        "Computed from one coefficient on the log scale, then exponentiated to the E(Y)-multiplier scale."
      } else {
        "Computed by combining coefficients on the log scale, then exponentiating to the E(Y)-multiplier scale."
      }
    )
  }

  invisible(NULL)
}

#' Build base-setting information for CI summaries
#'
#' @param mf Model frame.
#' @param numericReference Numeric reference choice.
#' @param predictorNames Predictor names.
#'
#' @return A list with base-row information.
#' @keywords internal
buildConfidenceIntervalBaseInfo = function(mf, numericReference, predictorNames) {

  baseRow = as.data.frame(mf[1, predictorNames, drop = FALSE], stringsAsFactors = FALSE)

  for (varName in predictorNames) {
    x = mf[[varName]]

    if (is.factor(x)) {
      baseRow[[varName]] = factor(levels(x)[1], levels = levels(x))
    } else if (is.numeric(x)) {
      if (identical(numericReference, "mean")) {
        baseRow[[varName]] = mean(x, na.rm = TRUE)
      } else {
        baseRow[[varName]] = 0
      }
    } else {
      baseRow[[varName]] = x[which(!is.na(x))[1]]
    }
  }

  baselineSettings = paste(
    vapply(
      predictorNames,
      function(varName) {
        formatBaseSettingForConfidenceIntervals(
          mf = mf,
          baseRow = baseRow,
          varName = varName,
          numericReference = numericReference
        )
      },
      character(1)
    ),
    collapse = "; "
  )

  list(
    baseRow = baseRow,
    baselineSettings = baselineSettings,
    numericReference = numericReference
  )
}

#' Complete one-row newdata for CI predictions
#'
#' Ensures that all predictors referenced by the fitted model are present in
#' the one-row \code{newData} passed to \code{predict()}, filling any omitted
#' columns from simple base settings taken from the model frame. This keeps the
#' CI builder robust when a row constructor only changes the focal predictors
#' for a teaching quantity.
#'
#' @param model A fitted model object.
#' @param newData One-row new-data frame that may omit some predictors.
#' @param numericReference Optional numeric reference choice. If omitted,
#'   it is chosen with \code{chooseModelNumericReference()}.
#' @param mf Optional model frame.
#' @param predictorNames Optional predictor names.
#'
#' @return A completed one-row data frame.
#' @keywords internal
completeConfidenceIntervalNewData = function(
    model,
    newData,
    numericReference = NULL,
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
    return(newData)
  }

  if (is.null(numericReference)) {
    numericReference = chooseModelNumericReference(
      model = model,
      mf = mf,
      predictorNames = predictorNames
    )
  }

  baseInfo = buildConfidenceIntervalBaseInfo(
    mf = mf,
    numericReference = numericReference,
    predictorNames = predictorNames
  )

  out = as.data.frame(newData, stringsAsFactors = FALSE)

  for (varName in predictorNames) {
    if (varName %in% names(out)) {
      next
    }

    out[[varName]] = baseInfo$baseRow[[varName]][1]
  }

  out = out[, predictorNames, drop = FALSE]

  for (varName in predictorNames) {
    x = mf[[varName]]

    if (is.factor(x)) {
      out[[varName]] = factor(as.character(out[[varName]]), levels = levels(x))
    } else if (is.numeric(x)) {
      out[[varName]] = as.numeric(out[[varName]])
    }
  }

  out
}

#' Build a prediction and CI on the linear predictor scale
#'
#' @param model A fitted model object.
#' @param newData One-row new-data frame.
#' @param level Confidence level.
#' @param numericReference Numeric reference choice used if 
#'   \code{newData} omits a numeric predictor.
#'
#' @return A list with fit, interval bounds, and non-zero weights.
#' @keywords internal
buildConfidenceIntervalPrediction = function(
    model,
    newData,
    level,
    numericReference = NULL
) {

  completedNewData = completeConfidenceIntervalNewData(
    model = model,
    newData = newData,
    numericReference = numericReference
  )

  predType = if (inherits(model, "glm")) "link" else "response"
  pred = predict(model, newdata = completedNewData, se.fit = TRUE, type = predType)

  eta = as.numeric(pred$fit)[1]
  seEta = as.numeric(pred$se.fit)[1]
  crit = getConfidenceIntervalCriticalValue(model = model, level = level)

  mm = model.matrix(delete.response(terms(model)), data = completedNewData)
  weights = as.numeric(mm[1, ])
  names(weights) = colnames(mm)
  weights = weights[abs(weights) > 1e-12]

  list(
    eta = eta,
    lowerEta = eta - crit * seEta,
    upperEta = eta + crit * seEta,
    weights = weights
  )
}

#' Build a CI for a linear combination of coefficients
#'
#' @param model A fitted model object.
#' @param weights Named coefficient weights.
#' @param level Confidence level.
#'
#' @return A list with estimate, bounds, and weights.
#' @keywords internal
buildLinearCombinationInterval = function(model, weights, level) {

  coefVec = coef(model)
  vc = vcov(model)
  fullWeights = setNames(rep(0, length(coefVec)), names(coefVec))
  fullWeights[names(weights)] = as.numeric(weights)

  estimate = sum(fullWeights * coefVec)
  variance = as.numeric(t(fullWeights) %*% vc %*% fullWeights)
  se = sqrt(variance)
  crit = getConfidenceIntervalCriticalValue(model = model, level = level)

  list(
    estimate = estimate,
    lower = estimate - crit * se,
    upper = estimate + crit * se,
    weights = weights[abs(weights) > 1e-12]
  )
}

#' Build coefficient weights for a simple interaction slope
#'
#' @param model A fitted model object.
#' @param factorName Factor predictor name.
#' @param factorLevel Selected factor level.
#' @param numericName Numeric predictor name.
#'
#' @return A named numeric vector.
#' @keywords internal
buildSimpleInteractionSlopeWeights = function(model, factorName, factorLevel, numericName) {

  coefNames = names(coef(model))
  weights = numeric(0)

  if (numericName %in% coefNames) {
    weights[numericName] = 1
  }

  factorObj = model.frame(model)[[factorName]]
  baseLevel = levels(factorObj)[1]

  if (identical(factorLevel, baseLevel)) {
    return(weights)
  }

  candidateNames = c(
    paste0(factorName, factorLevel, ":", numericName),
    paste0(numericName, ":", factorName, factorLevel)
  )
  interactionName = candidateNames[candidateNames %in% coefNames][1]

  if (!is.na(interactionName) && nzchar(interactionName)) {
    weights[interactionName] = 1
  }

  weights
}

#' Build CI notation labels for supported models
#'
#' @param model A fitted model object.
#' @param mf Model frame.
#'
#' @return A named list of notation labels.
#' @keywords internal
buildConfidenceIntervalNotation = function(model, mf) {

  responseName = names(mf)[1]
  response = mf[[1]]

  if (isSupportedLogisticModel(model = model)) {
    levs = levels(response)
    success = levs[length(levs)]
    failure = levs[1]

    return(list(
      probabilitySuccess = paste0("Pr(", responseName, " = ", success, ")"),
      probabilityFailure = paste0("Pr(", responseName, " = ", failure, ")"),
      oddsSuccess = paste0("Odds(", responseName, " = ", success, ")"),
      oddsFailure = paste0("Odds(", responseName, " = ", failure, ")"),
      responseScale = "response"
    ))
  }

  if (isSupportedPoissonModel(model = model)) {
    return(list(
      mean = "E(Y)",
      responseScale = "response"
    ))
  }

  list(
    mean = paste0("Expected ", responseName),
    responseScale = "response"
  )
}

#' Build a fitted-quantity label
#'
#' @param labelContext Context list.
#' @param notation Notation list.
#' @param quantityType Optional quantity type override.
#'
#' @return A character scalar.
#' @keywords internal
buildFittedQuantityLabel = function(labelContext, notation, quantityType = NULL) {

  if (is.null(quantityType)) {
    quantityType = if (!is.null(notation$mean)) {
      "mean"
    } else {
      "response"
    }
  }

  prefix = switch(
    quantityType,
    mean = notation$mean,
    response = notation$mean,
    probabilitySuccess = notation$probabilitySuccess,
    probabilityFailure = notation$probabilityFailure,
    oddsSuccess = notation$oddsSuccess,
    oddsFailure = notation$oddsFailure
  )

  if (identical(labelContext$type, "baseline")) {
    return(paste0(prefix, " at base settings"))
  }

  if (identical(labelContext$type, "factorLevel")) {
    return(paste0(prefix, " when ", labelContext$factorName, " = ", labelContext$level))
  }

  prefix
}

#' Build the default note for derived CI output
#'
#' @param mf Model frame.
#' @param numericReference Numeric reference choice.
#'
#' @return A character scalar or \code{NULL}.
#' @keywords internal
buildDerivedModeNote = function(mf, numericReference) {

  predictorNames = names(mf)[-1]
  factorNames = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]
  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  pieces = c(
    "Rows are tagged internally as fitted quantities or predictor effects so the app can show one teaching scale at a time."
  )

  if (length(factorNames) > 0) {
    factorText = paste(
      vapply(
        factorNames,
        function(varName) {
          paste0(varName, " = ", levels(mf[[varName]])[1])
        },
        character(1)
      ),
      collapse = "; "
    )
    pieces = c(pieces, paste0("Factor predictors use their base levels by default: ", factorText, "."))
  }

  if (length(numericNames) > 0 && !identical(numericReference, "context-dependent")) {
    numericText = paste(
      vapply(
        numericNames,
        function(varName) {
          x = mf[[varName]]
          x = x[!is.na(x)]

          if (length(x) == 0) {
            if (identical(numericReference, "zero")) {
              return(paste0(varName, " = 0 (all values missing)"))
            }

            return(paste0(varName, " = mean(", varName, ") = NA (all values missing)"))
          }

          rangeText = paste0(
            "[",
            formatConfidenceIntervalNumber(min(x)),
            ", ",
            formatConfidenceIntervalNumber(max(x)),
            "]"
          )

          if (identical(numericReference, "zero")) {
            paste0(varName, " = 0 (observed range ", rangeText, ")")
          } else {
            paste0(
              varName,
              " = mean(",
              varName,
              ") = ",
              formatConfidenceIntervalNumber(mean(x)),
              " (observed range ",
              rangeText,
              ")"
            )
          }
        },
        character(1)
      ),
      collapse = "; "
    )

    if (identical(numericReference, "zero")) {
      pieces = c(pieces, paste0("Numeric predictors are fixed at 0 for fitted-quantity rows: ", numericText, "."))
    } else {
      pieces = c(pieces, paste0("Numeric predictors are fixed at their means for fitted-quantity rows: ", numericText, "."))
    }
  }

  paste(pieces, collapse = " ")
}

#' Build text describing other predictors at base settings
#'
#' @param mf Model frame.
#' @param baseRow One-row base data frame.
#' @param excludeVarName Optional predictor to exclude.
#' @param numericReference Numeric reference choice.
#'
#' @return A character scalar.
#' @keywords internal
buildOtherBaseSettingsText = function(mf, baseRow, excludeVarName = NULL, numericReference) {

  predictorNames = names(mf)[-1]
  factorNames = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]
  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (!is.null(excludeVarName)) {
    factorNames = setdiff(factorNames, excludeVarName)
    numericNames = setdiff(numericNames, excludeVarName)
  }

  pieces = character(0)

  if (length(factorNames) > 0) {
    pieces = c(
      pieces,
      paste0(
        "Other factors fixed at base levels: ",
        paste(
          vapply(
            factorNames,
            function(varName) {
              paste0(varName, " = ", as.character(baseRow[[varName]][1]))
            },
            character(1)
          ),
          collapse = "; "
        )
      )
    )
  }

  if (length(numericNames) > 0) {
    pieces = c(
      pieces,
      paste0(
        "Other numeric predictors fixed at: ",
        paste(
          vapply(
            numericNames,
            function(varName) {
              x = mf[[varName]]
              x = x[!is.na(x)]

              if (identical(numericReference, "zero")) {
                if (length(x) == 0) {
                  return(paste0(varName, " = 0 (all values missing)"))
                }

                return(paste0(
                  varName,
                  " = 0 (observed range [",
                  formatConfidenceIntervalNumber(min(x)),
                  ", ",
                  formatConfidenceIntervalNumber(max(x)),
                  "])")
                )
              }

              if (length(x) == 0) {
                return(paste0(varName, " = mean(", varName, ") = NA (all values missing)"))
              }

              paste0(
                varName,
                " = mean(",
                varName,
                ") = ",
                formatConfidenceIntervalNumber(baseRow[[varName]][1]),
                " (observed range [",
                formatConfidenceIntervalNumber(min(x)),
                ", ",
                formatConfidenceIntervalNumber(max(x)),
                "])")
            },
            character(1)
          ),
          collapse = "; "
        )
      )
    )
  }

  if (length(pieces) == 0) {
    return("No other predictor settings are needed for this quantity.")
  }

  paste(pieces, collapse = ". ")
}

#' Format a base setting for display
#'
#' @param mf Model frame.
#' @param baseRow One-row base data frame.
#' @param varName Variable name.
#' @param numericReference Numeric reference choice.
#'
#' @return A character scalar.
#' @keywords internal
formatBaseSettingForConfidenceIntervals = function(mf, baseRow, varName, numericReference) {

  if (is.factor(mf[[varName]])) {
    return(paste0(varName, " = ", as.character(baseRow[[varName]][1])))
  }

  if (is.numeric(mf[[varName]]) && identical(numericReference, "mean")) {
    return(paste0(
      varName,
      " = mean(",
      varName,
      ") = ",
      formatConfidenceIntervalNumber(baseRow[[varName]][1])
    ))
  }

  paste0(varName, " = ", formatConfidenceIntervalNumber(baseRow[[varName]][1]))
}

#' Format a number for CI display text
#'
#' @param x A numeric value.
#'
#' @return A character scalar.
#' @keywords internal
formatConfidenceIntervalNumber = function(x) {
  format(round(as.numeric(x), 3), trim = TRUE)
}

#' Get the CI critical value for a fitted model
#'
#' @param model A fitted model object.
#' @param level Confidence level.
#'
#' @return A numeric scalar.
#' @keywords internal
getConfidenceIntervalCriticalValue = function(model, level) {

  if (inherits(model, "lm")) {
    return(qt(1 - (1 - level) / 2, df = model$df.residual))
  }

  qnorm(1 - (1 - level) / 2)
}

#' Test whether a model is a supported logistic GLM
#'
#' @param model A fitted model object.
#'
#' @return A logical scalar.
#' @keywords internal
isSupportedLogisticModel = function(model) {

  inherits(model, "glm") &&
    identical(model$family$family, "binomial") &&
    identical(model$family$link, "logit")
}

#' Test whether a model is a supported Poisson GLM
#'
#' @param model A fitted model object.
#'
#' @return A logical scalar.
#' @keywords internal
isSupportedPoissonModel = function(model) {

  inherits(model, "glm") &&
    identical(model$family$family, "poisson") &&
    identical(model$family$link, "log")
}

#' Enrich a CI table with framework-aware display metadata
#'
#' Adds internal columns that the app can use to filter and present
#' confidence-interval rows by model framework, teaching role, and display
#' scale. This keeps the calculation layer stable while making the returned
#' table easier to drive from UI controls.
#'
#' @param model A fitted model object.
#' @param ciTable A confidence-interval display table.
#'
#' @return A data frame with additional metadata columns.
#' @keywords internal
enrichConfidenceIntervalDisplayTable = function(model, ciTable) {

  if (is.null(ciTable) || !is.data.frame(ciTable) || nrow(ciTable) == 0) {
    return(ciTable)
  }

  out = ciTable
  modelFramework = detectConfidenceIntervalModelFramework(model = model)

  out$modelFramework = modelFramework
  out$rowRole = ifelse(out$ciSection %in% "effect", "covariateEffect", ifelse(out$ciSection %in% "coefficient", "coefficient", "fittedQuantity"))
  out$quantityType = ifelse(out$rowRole %in% "fittedQuantity", "Modelled outcome", ifelse(out$rowRole %in% "covariateEffect", "Predictor effect", "Coefficient"))
  out$displayScale = vapply(out$scale, mapConfidenceIntervalDisplayScale, character(1), modelFramework = modelFramework)
  out$outcomeLevel = vapply(out$quantity, extractConfidenceIntervalOutcomeLevel, character(1))
  out$isComplement = FALSE

  if (identical(modelFramework, "binomialLogit")) {
    response = model.frame(model)[[1]]
    if (is.factor(response) && length(levels(response)) >= 2) {
      failureLevel = levels(response)[1]
      out$isComplement = nzchar(out$outcomeLevel) & out$outcomeLevel %in% failureLevel
    }
  }

  out$primaryScale = vapply(out$scale, mapConfidenceIntervalPrimaryScale, character(1), modelFramework = modelFramework)
  out$secondaryScale = vapply(out$scale, mapConfidenceIntervalSecondaryScale, character(1), modelFramework = modelFramework)
  out$primaryEstimate = out$estimate
  out$primaryLower = out$lower
  out$primaryUpper = out$upper

  secondary = buildSecondaryConfidenceIntervalColumns(modelFramework = modelFramework, ciTable = out)
  out$secondaryEstimate = secondary$estimate
  out$secondaryLower = secondary$lower
  out$secondaryUpper = secondary$upper
  out$sortKey = buildConfidenceIntervalSortKey(out)

  rownames(out) = NULL
  out
}

#' Detect the teaching framework for CI display metadata
#'
#' @param model A fitted model object.
#'
#' @return A character scalar.
#' @keywords internal
detectConfidenceIntervalModelFramework = function(model) {

  if (isSupportedLogisticModel(model = model)) {
    return("binomialLogit")
  }

  if (isSupportedPoissonModel(model = model)) {
    return("poissonLog")
  }

  "lm"
}

#' Map a raw CI row scale to a display-scale identifier
#'
#' @param scale A raw row scale label.
#' @param modelFramework Teaching framework identifier.
#'
#' @return A character scalar.
#' @keywords internal
mapConfidenceIntervalDisplayScale = function(scale, modelFramework) {

  if (identical(modelFramework, "binomialLogit")) {
    return(switch(
      scale,
      probability = "probability",
      odds = "odds",
      "odds multiplier" = "oddsMultiplier",
      coefficient = "coefficient",
      scale
    ))
  }

  if (identical(modelFramework, "poissonLog")) {
    return(switch(
      scale,
      "expected value" = "expectedValue",
      "E(Y) multiplier" = "expectedValueMultiplier",
      coefficient = "coefficient",
      scale
    ))
  }

  switch(
    scale,
    response = "fittedValue",
    coefficient = "coefficient",
    "fittedValue"
  )
}

#' Map a raw CI row scale to a primary teaching scale label
#'
#' @param scale A raw row scale label.
#' @param modelFramework Teaching framework identifier.
#'
#' @return A character scalar.
#' @keywords internal
mapConfidenceIntervalPrimaryScale = function(scale, modelFramework) {

  if (identical(modelFramework, "binomialLogit")) {
    return(switch(
      scale,
      probability = "probability",
      odds = "odds",
      "odds multiplier" = "odds multiplier",
      coefficient = "log-odds coefficient",
      scale
    ))
  }

  if (identical(modelFramework, "poissonLog")) {
    return(switch(
      scale,
      "expected value" = "expected value",
      "E(Y) multiplier" = "E(Y) multiplier",
      coefficient = "log-mean coefficient",
      scale
    ))
  }

  switch(
    scale,
    response = "response",
    coefficient = "coefficient",
    scale
  )
}

#' Map a raw CI row scale to a secondary teaching scale label
#'
#' @param scale A raw row scale label.
#' @param modelFramework Teaching framework identifier.
#'
#' @return A character scalar.
#' @keywords internal
mapConfidenceIntervalSecondaryScale = function(scale, modelFramework) {

  if (identical(modelFramework, "binomialLogit")) {
    return(switch(
      scale,
      probability = "odds",
      odds = "log-odds",
      "odds multiplier" = "log-odds coefficient",
      coefficient = "odds multiplier",
      ""
    ))
  }

  if (identical(modelFramework, "poissonLog")) {
    return(switch(
      scale,
      "expected value" = "log mean",
      "E(Y) multiplier" = "log multiplier",
      coefficient = "E(Y) multiplier",
      ""
    ))
  }

  ""
}

#' Extract the outcome level named in a CI quantity label
#'
#' @param quantity A quantity label.
#'
#' @return A character scalar.
#' @keywords internal
extractConfidenceIntervalOutcomeLevel = function(quantity) {

  match = regexec("= ([^)]+)\\)", quantity)
  pieces = regmatches(quantity, match)[[1]]

  if (length(pieces) >= 2) {
    return(pieces[2])
  }

  ""
}

#' Build secondary-scale columns for a CI table
#'
#' @param modelFramework Teaching framework identifier.
#' @param ciTable Confidence-interval table with primary values.
#'
#' @return A list of numeric vectors.
#' @keywords internal
buildSecondaryConfidenceIntervalColumns = function(modelFramework, ciTable) {

  n = nrow(ciTable)
  estimate = rep(NA_real_, n)
  lower = rep(NA_real_, n)
  upper = rep(NA_real_, n)

  if (identical(modelFramework, "binomialLogit")) {
    isProbability = ciTable$scale %in% "probability"
    isOdds = ciTable$scale %in% "odds"
    isOddsMultiplier = ciTable$scale %in% "odds multiplier"
    isCoefficient = ciTable$scale %in% "coefficient"

    estimate[isProbability] = ciTable$estimate[isProbability] / (1 - ciTable$estimate[isProbability])
    lower[isProbability] = ciTable$lower[isProbability] / (1 - ciTable$lower[isProbability])
    upper[isProbability] = ciTable$upper[isProbability] / (1 - ciTable$upper[isProbability])

    estimate[isOdds | isOddsMultiplier] = log(ciTable$estimate[isOdds | isOddsMultiplier])
    lower[isOdds | isOddsMultiplier] = log(ciTable$lower[isOdds | isOddsMultiplier])
    upper[isOdds | isOddsMultiplier] = log(ciTable$upper[isOdds | isOddsMultiplier])

    estimate[isCoefficient] = exp(ciTable$estimate[isCoefficient])
    lower[isCoefficient] = exp(ciTable$lower[isCoefficient])
    upper[isCoefficient] = exp(ciTable$upper[isCoefficient])
  }

  if (identical(modelFramework, "poissonLog")) {
    isExpected = ciTable$scale %in% "expected value"
    isMultiplier = ciTable$scale %in% "E(Y) multiplier"
    isCoefficient = ciTable$scale %in% "coefficient"

    estimate[isExpected | isMultiplier] = log(ciTable$estimate[isExpected | isMultiplier])
    lower[isExpected | isMultiplier] = log(ciTable$lower[isExpected | isMultiplier])
    upper[isExpected | isMultiplier] = log(ciTable$upper[isExpected | isMultiplier])

    estimate[isCoefficient] = exp(ciTable$estimate[isCoefficient])
    lower[isCoefficient] = exp(ciTable$lower[isCoefficient])
    upper[isCoefficient] = exp(ciTable$upper[isCoefficient])
  }

  list(
    estimate = round(estimate, 3),
    lower = round(lower, 3),
    upper = round(upper, 3)
  )
}

#' Build a stable sort key for a CI display table
#'
#' @param ciTable Confidence-interval table with metadata columns.
#'
#' @return An integer vector.
#' @keywords internal
buildConfidenceIntervalSortKey = function(ciTable) {

  rowBase = ifelse(ciTable$rowRole %in% "fittedQuantity", 1000L, ifelse(ciTable$rowRole %in% "covariateEffect", 2000L, 3000L))
  scaleOffset = match(ciTable$displayScale, unique(ciTable$displayScale))
  scaleOffset[is.na(scaleOffset)] = 99L

  rowBase + (scaleOffset * 100L) + seq_len(nrow(ciTable))
}

#' Insert pedagogical section-break rows into a CI display table
#'
#' This helper is retained for backward compatibility, but the Stage 4 display
#' redesign no longer inserts visible separator rows into the returned table.
#'
#' @param ciTable A confidence-interval display table.
#'
#' @return The input data frame, unchanged apart from row names.
#' @keywords internal
insertCiSectionBreakRows = function(ciTable) {

  if (is.null(ciTable) || !is.data.frame(ciTable)) {
    return(ciTable)
  }

  rownames(ciTable) = NULL
  ciTable
}

#' Build a coefficient-only confidence-interval table
#'
#' @param model A fitted model object.
#' @param level Confidence level.
#'
#' @return A list in the same format as \code{buildModelConfidenceIntervalData()}.
#' @keywords internal
buildCoefficientOnlyConfidenceIntervalData = function(model, level = 0.95) {

  coefVec = coef(model)
  vc = vcov(model)
  se = sqrt(diag(vc))
  crit = getConfidenceIntervalCriticalValue(model = model, level = level)

  lower = coefVec - crit * se
  upper = coefVec + crit * se

  out = data.frame(
    ciSection = "coefficient",
    quantity = names(coefVec),
    estimate = round(as.numeric(coefVec), 3),
    lower = round(as.numeric(lower), 3),
    upper = round(as.numeric(upper), 3),
    scale = "coefficient",
    stringsAsFactors = FALSE
  )
  out = enrichConfidenceIntervalDisplayTable(
    model = model,
    ciTable = out
  )

  details = lapply(names(coefVec), function(name) {
    list(
      label = name,
      quantity = name,
      section = "coefficient",
      settings = "Coefficient interval with no additional predictor settings.",
      builtFrom = buildLinearCombinationText(setNames(1, name)),
      varianceFormula = buildLinearCombinationVarianceText(setNames(1, name)),
      scaleNote = "Computed directly from a single coefficient."
    )
  })

  list(
    table = out,
    details = details,
    note = NULL,
    teachingNote = buildModelConfidenceIntervalTeachingNote(model = model),
    vcovTable = round(vcov(model), 3),
    mode = "coefficient"
  )
}

#' Build readable text for a linear combination of coefficients
#'
#' @param nonZero Named numeric vector of non-zero design weights.
#'
#' @return A character scalar.
#' @keywords internal
buildLinearCombinationText = function(nonZero) {

  pieces = mapply(
    function(weight, name) {
      if (identical(name, "(Intercept)")) {
        baseName = "Intercept"
      } else {
        baseName = name
      }

      if (abs(weight - 1) < 1e-12) {
        baseName
      } else {
        paste0(format(round(weight, 3), trim = TRUE), " x ", baseName)
      }
    },
    weight = as.numeric(nonZero),
    name = names(nonZero),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  paste(pieces, collapse = " + ")
}

#' Build readable variance text for a linear combination
#'
#' @param nonZero Named numeric vector of non-zero design weights.
#'
#' @return A character scalar.
#' @keywords internal
buildLinearCombinationVarianceText = function(nonZero) {

  namesClean = ifelse(names(nonZero) == "(Intercept)", "Intercept", names(nonZero))
  weights = as.numeric(nonZero)

  if (length(weights) == 1) {
    return(paste0("Var(", namesClean[1], ")"))
  }

  varTerms = vapply(
    seq_along(weights),
    function(i) {
      weight = weights[i]
      name = namesClean[i]

      if (abs(weight - 1) < 1e-12) {
        paste0("Var(", name, ")")
      } else {
        paste0(format(round(weight^2, 3), trim = TRUE), " Var(", name, ")")
      }
    },
    character(1)
  )

  covPairs = utils::combn(seq_along(weights), 2, simplify = FALSE)
  covTerms = vapply(
    covPairs,
    function(idx) {
      i = idx[1]
      j = idx[2]
      coeff = 2 * weights[i] * weights[j]
      coeffText = format(round(coeff, 3), trim = TRUE)

      if (abs(coeff - 1) < 1e-12) {
        paste0("Cov(", namesClean[i], ", ", namesClean[j], ")")
      } else {
        paste0(coeffText, " Cov(", namesClean[i], ", ", namesClean[j], ")")
      }
    },
    character(1)
  )

  paste(c(varTerms, covTerms), collapse = " + ")
}

#' Build a short teaching note for confidence-interval drill-down
#'
#' @param model A fitted model object.
#'
#' @return A character scalar.
#' @keywords internal
buildModelConfidenceIntervalTeachingNote = function(model) {

  if (inherits(model, "glm")) {
    linkText = paste0("For this model, intervals are first computed on the ", model$family$link, " scale and then transformed back.")
  } else {
    linkText = "For this model, intervals are computed directly on the response scale unless a row says otherwise."
  }

  paste(
    "Variance is the square of the standard error, so SE^2 gives the variance used inside each confidence interval calculation.",
    "When a displayed quantity combines more than one coefficient, covariance terms appear because those coefficient estimates are not treated as independent.",
    linkText
  )
}
