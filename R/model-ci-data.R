#' Build interpretable confidence-interval output for a fitted model
#'
#' Creates a compact table of confidence intervals for quantities that are
#' easier for students to interpret than a raw coefficient table. The helper
#' builds intervals for fitted values at simple predictor settings and, where
#' helpful, simple contrasts such as one-unit numeric changes or factor-level
#' comparisons.
#'
#' For binomial logit models, the displayed rows use the probability and odds
#' scales. For log-link count models, multiplicative count effects are shown.
#' For identity-link models, rows stay on the response scale.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param level Confidence level. Defaults to `0.95`.
#' @param numericReference How numeric predictors should be fixed when building
#'   teaching rows. One of `"mean"` or `"zero"`. Defaults to `"mean"`.
#'   The app UI uses `"zero"` so the displayed rows line up with explanations
#'   phrased at zero-valued numeric predictors.
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

  factorNames = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]
  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  familyObj = if (inherits(model, "glm")) {
    family(model)
  } else {
    NULL
  }

  isBinomialLogit = inherits(model, "glm") &&
    identical(familyObj$family, "binomial") &&
    identical(familyObj$link, "logit")

  isPoissonLog = inherits(model, "glm") &&
    identical(familyObj$family, "poisson") &&
    identical(familyObj$link, "log")

  outcomeLabels = if (isBinomialLogit) {
    getBinomialOutcomeLabels(model)
  } else {
    NULL
  }

  tt = delete.response(terms(model))
  crit = computeModelCriticalValue(model = model, level = level)
  baseRow = buildModelBaseRow(
    mf = mf,
    predictorNames = predictorNames,
    numericReference = numericReference
  )

  rows = list()
  details = list()

  appendOutputRow = function(label, estimate, lower, upper, scaleLabel, settingsText, builtFrom, varianceFormula, scaleNote) {

    rows[[length(rows) + 1]] <<- data.frame(
      quantity = label,
      estimate = round(estimate, 3),
      lower = round(lower, 3),
      upper = round(upper, 3),
      scale = scaleLabel,
      stringsAsFactors = FALSE
    )

    details[[length(details) + 1]] <<- list(
      label = label,
      quantity = label,
      settings = settingsText,
      builtFrom = builtFrom,
      varianceFormula = varianceFormula,
      scaleNote = scaleNote
    )
  }

  addPredictionRows = function(newData, conditionLabel, settingsText) {

    predType = if (inherits(model, "glm")) "link" else "response"
    pred = predict(model, newdata = newData, se.fit = TRUE, type = predType)

    eta = as.numeric(pred$fit)[1]
    seEta = as.numeric(pred$se.fit)[1]
    lowerEta = eta - crit * seEta
    upperEta = eta + crit * seEta

    mm = model.matrix(tt, data = newData)
    xVec = as.numeric(mm[1, ])
    names(xVec) = colnames(mm)
    nonZero = xVec[abs(xVec) > 1e-12]

    if (isBinomialLogit) {
      successProbLabel = formatBinomialProbabilityLabel(model, outcome = "success")
      successOddsLabel = formatBinomialOddsLabel(model, outcome = "success")

      appendOutputRow(
        label = paste0(successProbLabel, " ", conditionLabel),
        estimate = familyObj$linkinv(eta),
        lower = familyObj$linkinv(lowerEta),
        upper = familyObj$linkinv(upperEta),
        scaleLabel = "probability",
        settingsText = settingsText,
        builtFrom = buildLinearCombinationText(nonZero),
        varianceFormula = buildLinearCombinationVarianceText(nonZero),
        scaleNote = "Computed on the logit scale for the chosen setting, then transformed to the probability scale."
      )

      appendOutputRow(
        label = paste0(successOddsLabel, " ", conditionLabel),
        estimate = exp(eta),
        lower = exp(lowerEta),
        upper = exp(upperEta),
        scaleLabel = "odds",
        settingsText = settingsText,
        builtFrom = buildLinearCombinationText(nonZero),
        varianceFormula = buildLinearCombinationVarianceText(nonZero),
        scaleNote = "Computed on the logit scale for the chosen setting, then exponentiated to the odds scale."
      )

      return(invisible(NULL))
    }

    if (isPoissonLog) {
      appendOutputRow(
        label = paste0("Expected count ", conditionLabel),
        estimate = familyObj$linkinv(eta),
        lower = familyObj$linkinv(lowerEta),
        upper = familyObj$linkinv(upperEta),
        scaleLabel = "expected count",
        settingsText = settingsText,
        builtFrom = buildLinearCombinationText(nonZero),
        varianceFormula = buildLinearCombinationVarianceText(nonZero),
        scaleNote = "Computed on the log scale for the chosen setting, then transformed back to the expected-count scale."
      )

      return(invisible(NULL))
    }

    if (inherits(model, "glm") && !identical(familyObj$link, "identity")) {
      appendOutputRow(
        label = paste0("Expected response ", conditionLabel),
        estimate = familyObj$linkinv(eta),
        lower = familyObj$linkinv(lowerEta),
        upper = familyObj$linkinv(upperEta),
        scaleLabel = "response",
        settingsText = settingsText,
        builtFrom = buildLinearCombinationText(nonZero),
        varianceFormula = buildLinearCombinationVarianceText(nonZero),
        scaleNote = paste(
          "Computed on the", familyObj$link, "scale for the chosen setting,",
          "then transformed back to the response scale."
        )
      )

      return(invisible(NULL))
    }

    appendOutputRow(
      label = paste0("Expected ", names(mf)[1], " ", conditionLabel),
      estimate = eta,
      lower = lowerEta,
      upper = upperEta,
      scaleLabel = "response",
      settingsText = settingsText,
      builtFrom = buildLinearCombinationText(nonZero),
      varianceFormula = buildLinearCombinationVarianceText(nonZero),
      scaleNote = "Computed directly on the response scale for the chosen setting."
    )
  }

  addContrastRow = function(label, highData, lowData, settingsText) {

    highMatrix = model.matrix(tt, data = highData)
    lowMatrix = model.matrix(tt, data = lowData)

    weightVec = as.numeric(highMatrix[1, ] - lowMatrix[1, ])
    names(weightVec) = colnames(highMatrix)

    interval = computeLinearCombinationInterval(
      model = model,
      weightVec = weightVec,
      crit = crit
    )

    nonZero = weightVec[abs(weightVec) > 1e-12]

    if (isBinomialLogit) {
      appendOutputRow(
        label = label,
        estimate = exp(interval$estimate),
        lower = exp(interval$lower),
        upper = exp(interval$upper),
        scaleLabel = "odds multiplier",
        settingsText = settingsText,
        builtFrom = buildLinearCombinationText(nonZero),
        varianceFormula = buildLinearCombinationVarianceText(nonZero),
        scaleNote = "Computed as a contrast on the logit scale, then exponentiated to the odds-multiplier scale."
      )

      return(invisible(NULL))
    }

    if (isPoissonLog) {
      appendOutputRow(
        label = label,
        estimate = exp(interval$estimate),
        lower = exp(interval$lower),
        upper = exp(interval$upper),
        scaleLabel = "multiplier",
        settingsText = settingsText,
        builtFrom = buildLinearCombinationText(nonZero),
        varianceFormula = buildLinearCombinationVarianceText(nonZero),
        scaleNote = "Computed as a contrast on the log scale, then exponentiated to the expected-count multiplier scale."
      )

      return(invisible(NULL))
    }

    scaleLabel = if (inherits(model, "glm") && !identical(familyObj$link, "identity")) {
      "link"
    } else {
      "response"
    }

    appendOutputRow(
      label = label,
      estimate = interval$estimate,
      lower = interval$lower,
      upper = interval$upper,
      scaleLabel = scaleLabel,
      settingsText = settingsText,
      builtFrom = buildLinearCombinationText(nonZero),
      varianceFormula = buildLinearCombinationVarianceText(nonZero),
      scaleNote = "Computed directly from a linear combination of coefficients."
    )
  }

  factorGrid = buildFactorSettingGrid(mf = mf, factorNames = factorNames)

  if (nrow(factorGrid) == 0) {
    factorGrid = data.frame(.placeholder = 1)
  }

  for (i in seq_len(nrow(factorGrid))) {
    newData = baseRow

    if (length(factorNames) > 0) {
      for (varName in factorNames) {
        newData[[varName]] = factor(
          as.character(factorGrid[[varName]][i]),
          levels = levels(mf[[varName]])
        )
      }
    }

    conditionLabel = buildPredictionConditionLabel(
      factorSettingRow = if (length(factorNames) > 0) factorGrid[i, factorNames, drop = FALSE] else NULL
    )

    settingsText = buildPredictionSettingsText(
      newData = newData,
      predictorNames = predictorNames,
      numericReference = numericReference,
      responseName = names(mf)[1]
    )

    addPredictionRows(
      newData = newData,
      conditionLabel = conditionLabel,
      settingsText = settingsText
    )
  }

  for (varName in factorNames) {
    refLevel = levels(mf[[varName]])[1]
    otherFactorNames = setdiff(factorNames, varName)
    otherFactorGrid = buildFactorSettingGrid(mf = mf, factorNames = otherFactorNames)

    if (nrow(otherFactorGrid) == 0) {
      otherFactorGrid = data.frame(.placeholder = 1)
    }

    for (lvl in levels(mf[[varName]])[-1]) {
      for (i in seq_len(nrow(otherFactorGrid))) {
        lowData = baseRow
        highData = baseRow

        lowData[[varName]] = factor(refLevel, levels = levels(mf[[varName]]))
        highData[[varName]] = factor(lvl, levels = levels(mf[[varName]]))

        if (length(otherFactorNames) > 0) {
          for (otherName in otherFactorNames) {
            otherValue = as.character(otherFactorGrid[[otherName]][i])
            lowData[[otherName]] = factor(otherValue, levels = levels(mf[[otherName]]))
            highData[[otherName]] = factor(otherValue, levels = levels(mf[[otherName]]))
          }
        }

        conditionSuffix = buildContrastConditionSuffix(
          factorSettingRow = if (length(otherFactorNames) > 0) otherFactorGrid[i, otherFactorNames, drop = FALSE] else NULL
        )

        if (isBinomialLogit) {
          label = paste0(
            formatBinomialOddsLabel(model, outcome = "success"),
            " multiplier for ", varName, " = ", lvl, " rather than ", refLevel,
            conditionSuffix
          )
        } else if (isPoissonLog) {
          label = paste0(
            "Expected-count multiplier for ", varName, " = ", lvl,
            " rather than ", refLevel,
            conditionSuffix
          )
        } else {
          label = paste0(
            "Change in ", names(mf)[1], " for ", varName, " = ", lvl,
            " rather than ", refLevel,
            conditionSuffix
          )
        }

        settingsText = paste0(
          "Compare ", varName, " = ", lvl, " to ", varName, " = ", refLevel, ". ",
          buildOtherPredictorSettingsText(
            model = model,
            newData = lowData,
            excludePredictor = varName,
            numericReference = numericReference
          )
        )

        addContrastRow(
          label = label,
          highData = highData,
          lowData = lowData,
          settingsText = settingsText
        )
      }
    }
  }

  for (varName in numericNames) {
    for (i in seq_len(nrow(factorGrid))) {
      lowData = baseRow
      highData = baseRow

      if (length(factorNames) > 0) {
        for (factorName in factorNames) {
          factorValue = as.character(factorGrid[[factorName]][i])
          lowData[[factorName]] = factor(factorValue, levels = levels(mf[[factorName]]))
          highData[[factorName]] = factor(factorValue, levels = levels(mf[[factorName]]))
        }
      }

      highData[[varName]] = as.numeric(lowData[[varName]]) + 1

      conditionSuffix = buildContrastConditionSuffix(
        factorSettingRow = if (length(factorNames) > 0) factorGrid[i, factorNames, drop = FALSE] else NULL
      )

      if (isBinomialLogit) {
        label = paste0(
          formatBinomialOddsLabel(model, outcome = "success"),
          " multiplier for a 1-unit increase in ", varName,
          conditionSuffix
        )
      } else if (isPoissonLog) {
        label = paste0(
          "Expected-count multiplier for a 1-unit increase in ", varName,
          conditionSuffix
        )
      } else {
        label = paste0(
          "Change in ", names(mf)[1], " for a 1-unit increase in ", varName,
          conditionSuffix
        )
      }

      settingsText = paste0(
        "Compare two otherwise identical settings that differ by 1 unit in ", varName, ". ",
        buildOtherPredictorSettingsText(
          model = model,
          newData = lowData,
          excludePredictor = varName,
          numericReference = numericReference
        )
      )

      addContrastRow(
        label = label,
        highData = highData,
        lowData = lowData,
        settingsText = settingsText
      )
    }
  }

  termLabels = attr(terms(model), "term.labels")
  hasInteractions = any(grepl(":", termLabels, fixed = TRUE))

  noteParts = character(0)

  if (hasInteractions) {
    noteParts = c(
      noteParts,
      "This model contains interaction terms, so several rows depend on the chosen settings for the other predictors."
    )
  }

  if (length(factorNames) > 0) {
    noteParts = c(
      noteParts,
      "Rows labelled with probabilities or odds are evaluated at the displayed factor settings."
    )
  }

  if (length(numericNames) > 0) {
    if (identical(numericReference, "zero")) {
      noteParts = c(noteParts, "Numeric predictors not named in a row are fixed at 0.")
    } else {
      noteParts = c(noteParts, "Numeric predictors not named in a row are fixed at their means.")
    }
  }

  list(
    table = do.call(rbind, rows),
    details = details,
    note = if (length(noteParts) > 0) paste(noteParts, collapse = " ") else NULL,
    teachingNote = teachingNote,
    vcovTable = vcovTable,
    mode = "derived"
  )
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
  crit = computeModelCriticalValue(model = model, level = level)

  lower = coefVec - crit * se
  upper = coefVec + crit * se

  out = data.frame(
    quantity = names(coefVec),
    estimate = round(as.numeric(coefVec), 3),
    lower = round(as.numeric(lower), 3),
    upper = round(as.numeric(upper), 3),
    scale = "coefficient",
    stringsAsFactors = FALSE
  )

  details = lapply(names(coefVec), function(name) {
    list(
      label = name,
      quantity = name,
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

#' Compute the critical value used in model confidence intervals
#'
#' @param model A fitted model object.
#' @param level Confidence level.
#'
#' @return A single numeric critical value.
#' @keywords internal
computeModelCriticalValue = function(model, level) {

  if (inherits(model, "lm")) {
    return(qt(1 - (1 - level) / 2, df = model$df.residual))
  }

  qnorm(1 - (1 - level) / 2)
}

#' Compute an interval for a linear combination of coefficients
#'
#' @param model A fitted model object.
#' @param weightVec Named numeric vector of coefficient weights.
#' @param crit Critical value for the desired confidence level.
#'
#' @return A list with `estimate`, `lower`, and `upper` on the linear-combination scale.
#' @keywords internal
computeLinearCombinationInterval = function(model, weightVec, crit) {

  coefVec = coef(model)
  vc = vcov(model)

  fullWeights = rep(0, length(coefVec))
  names(fullWeights) = names(coefVec)
  fullWeights[names(weightVec)] = weightVec

  estimate = sum(fullWeights * coefVec)
  variance = as.numeric(t(fullWeights) %*% vc %*% fullWeights)
  se = sqrt(pmax(variance, 0))

  list(
    estimate = estimate,
    lower = estimate - crit * se,
    upper = estimate + crit * se
  )
}

#' Build a base predictor row for teaching summaries
#'
#' @param mf Model frame.
#' @param predictorNames Character vector of predictor names.
#' @param numericReference Either `"mean"` or `"zero"`.
#'
#' @return A one-row data frame.
#' @keywords internal
buildModelBaseRow = function(mf, predictorNames, numericReference) {

  out = as.data.frame(mf[1, predictorNames, drop = FALSE], stringsAsFactors = FALSE)

  for (varName in predictorNames) {
    x = mf[[varName]]

    if (is.factor(x)) {
      out[[varName]] = factor(levels(x)[1], levels = levels(x))
    } else if (is.numeric(x)) {
      out[[varName]] = if (identical(numericReference, "mean")) {
        mean(x, na.rm = TRUE)
      } else {
        0
      }
    } else {
      out[[varName]] = x[which(!is.na(x))[1]]
    }
  }

  out
}

#' Build a grid of factor settings for teaching rows
#'
#' @param mf Model frame.
#' @param factorNames Character vector of factor predictor names.
#'
#' @return A data frame of factor settings.
#' @keywords internal
buildFactorSettingGrid = function(mf, factorNames) {

  if (length(factorNames) == 0) {
    return(data.frame())
  }

  gridList = lapply(
    factorNames,
    function(varName) {
      factor(levels(mf[[varName]]), levels = levels(mf[[varName]]))
    }
  )
  names(gridList) = factorNames

  do.call(expand.grid, c(gridList, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
}

#' Build a short condition label for a fitted-value row
#'
#' @param factorSettingRow A one-row data frame of factor settings, or `NULL`.
#'
#' @return A character scalar beginning with `"when ..."` or `"at base settings"`.
#' @keywords internal
buildPredictionConditionLabel = function(factorSettingRow) {

  if (is.null(factorSettingRow) || ncol(factorSettingRow) == 0) {
    return("at base settings")
  }

  paste0(
    "when ",
    paste(
      paste0(names(factorSettingRow), " = ", vapply(factorSettingRow, as.character, character(1))),
      collapse = ", "
    )
  )
}

#' Build a short condition suffix for a contrast label
#'
#' @param factorSettingRow A one-row data frame of factor settings, or `NULL`.
#'
#' @return A character scalar that is either empty or begins with `" when ..."`.
#' @keywords internal
buildContrastConditionSuffix = function(factorSettingRow) {

  if (is.null(factorSettingRow) || ncol(factorSettingRow) == 0) {
    return("")
  }

  paste0(
    " when ",
    paste(
      paste0(names(factorSettingRow), " = ", vapply(factorSettingRow, as.character, character(1))),
      collapse = ", "
    )
  )
}

#' Build readable predictor-setting text for a fitted-value row
#'
#' @param newData One-row predictor data frame.
#' @param predictorNames Character vector of predictor names.
#' @param numericReference Either `"mean"` or `"zero"`.
#' @param responseName Name of the response variable.
#'
#' @return A character scalar.
#' @keywords internal
buildPredictionSettingsText = function(newData, predictorNames, numericReference, responseName) {

  pieces = vapply(
    predictorNames,
    function(varName) {
      value = newData[[varName]][1]

      if (is.factor(newData[[varName]])) {
        return(paste0(varName, " = ", as.character(value)))
      }

      if (is.numeric(value) && identical(numericReference, "mean")) {
        return(paste0(varName, " = mean(", varName, ") = ", format(round(as.numeric(value), 3), trim = TRUE)))
      }

      paste0(varName, " = ", format(round(as.numeric(value), 3), trim = TRUE))
    },
    character(1)
  )

  paste0("Fitted ", responseName, " evaluated at: ", paste(pieces, collapse = "; "))
}

#' Build readable text for fixed predictors in a contrast row
#'
#' @param model A fitted model object.
#' @param newData One-row predictor data frame.
#' @param excludePredictor Predictor being contrasted.
#' @param numericReference Either `"mean"` or `"zero"`.
#'
#' @return A character scalar.
#' @keywords internal
buildOtherPredictorSettingsText = function(model, newData, excludePredictor, numericReference) {

  mf = model.frame(model)
  predictorNames = setdiff(names(mf)[-1], excludePredictor)

  if (length(predictorNames) == 0) {
    return("No other predictor settings are needed for this contrast.")
  }

  pieces = vapply(
    predictorNames,
    function(varName) {
      value = newData[[varName]][1]

      if (is.factor(mf[[varName]])) {
        return(paste0(varName, " = ", as.character(value)))
      }

      if (identical(numericReference, "mean")) {
        return(paste0(varName, " = mean(", varName, ") = ", format(round(as.numeric(value), 3), trim = TRUE)))
      }

      paste0(varName, " = ", format(round(as.numeric(value), 3), trim = TRUE))
    },
    character(1)
  )

  paste0("Other predictors fixed at: ", paste(pieces, collapse = "; "))
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
    linkText = paste0("For this model, intervals are first computed on the ", model$family$link, " scale and then transformed back when needed.")
  } else {
    linkText = "For this model, intervals are computed directly on the response scale unless a row says otherwise."
  }

  paste(
    "Variance is the square of the standard error, so SE^2 gives the variance used inside each confidence interval calculation.",
    "When a displayed quantity combines more than one coefficient, covariance terms appear because those coefficient estimates are not treated as independent.",
    linkText
  )
}
