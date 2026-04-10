#' Build interpretable confidence-interval output for a fitted model
#'
#' Creates a compact table of confidence intervals for quantities that are
#' easier for students to interpret than a raw coefficient table. For models
#' without interaction terms, the helper builds intervals for expected values
#' at simple base settings and for one-unit changes in numeric predictors.
#' It also records row-by-row explanation material in a labelled structure that
#' can be shown on demand in the app.
#'
#' When the model contains interaction terms, the helper falls back to a raw
#' coefficient-interval table because simple marginal quantities depend on the
#' values of other predictors.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param level Confidence level. Defaults to `0.95`.
#' @param numericReference How numeric predictors should be fixed when building
#'   expected-value rows. One of `"mean"` or `"zero"`. Defaults to `"mean"`.
#'   The app UI uses `"zero"` so the displayed expected values line up with
#'   explanations phrased at zero-valued numeric predictors.
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
  tt = terms(model)
  termLabels = attr(tt, "term.labels")
  if (is.null(termLabels)) {
    termLabels = character(0)
  }
  hasInteractions = any(grepl(":", termLabels, fixed = TRUE))

  vcovTable = round(vcov(model), 3)
  teachingNote = buildModelConfidenceIntervalTeachingNote(model = model)

  if (hasInteractions) {
    out = buildCoefficientOnlyConfidenceIntervalData(model = model, level = level)
    out$note = paste(
      "This model contains interaction terms, so simple one-row teaching summaries",
      "depend on the values chosen for the other predictors.",
      "Use the table first, then drill down on a selected row if needed."
    )
    out$teachingNote = teachingNote
    out$vcovTable = vcovTable
    return(out)
  }

  predictorNames = names(mf)[-1]
  if (length(predictorNames) == 0) {
    out = buildCoefficientOnlyConfidenceIntervalData(model = model, level = level)
    out$note = "This model has no predictors, so the confidence-interval table shows the intercept only."
    out$teachingNote = teachingNote
    out$vcovTable = vcovTable
    return(out)
  }

  familyObj = if (inherits(model, "glm")) {
    family(model)
  } else {
    NULL
  }

  predType = if (inherits(model, "glm")) "link" else "response"

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

  rows = list()
  details = list()

  getResponseScaleLabel = function() {
    if (is.null(familyObj) || identical(familyObj$link, "identity")) {
      return("response")
    }

    if (identical(familyObj$family, "binomial")) {
      return("probability")
    }

    if (identical(familyObj$family, "poisson")) {
      return("expected count")
    }

    "response"
  }

  formatBaseSetting = function(varName, value) {
    if (is.factor(mf[[varName]])) {
      paste0(varName, " = ", as.character(value))
    } else if (is.numeric(mf[[varName]]) && identical(numericReference, "mean")) {
      paste0(varName, " = mean(", varName, ") = ", format(round(as.numeric(value), 3), trim = TRUE))
    } else {
      paste0(varName, " = ", format(round(as.numeric(value), 3), trim = TRUE))
    }
  }

  buildOtherBaseLevelsText = function(excludeVarName = NULL) {
    otherFactorNames = predictorNames[vapply(mf[predictorNames], is.factor, logical(1))]
    if (!is.null(excludeVarName)) {
      otherFactorNames = setdiff(otherFactorNames, excludeVarName)
    }

    pieces = character(0)

    if (length(otherFactorNames) > 0) {
      factorText = paste(
        vapply(
          otherFactorNames,
          function(varName) {
            paste0(varName, " = ", as.character(baseRow[[varName]][1]))
          },
          character(1)
        ),
        collapse = "; "
      )
      pieces = c(pieces, paste0("Other factors fixed at base levels: ", factorText))
    }

    otherNumericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]
    if (!is.null(excludeVarName)) {
      otherNumericNames = setdiff(otherNumericNames, excludeVarName)
    }

    if (length(otherNumericNames) > 0) {
      if (identical(numericReference, "zero")) {
        numericText = paste0(otherNumericNames, " = 0", collapse = "; ")
      } else {
        numericText = paste(
          vapply(
            otherNumericNames,
            function(varName) {
              paste0(varName, " = mean(", varName, ") = ", format(round(as.numeric(baseRow[[varName]][1]), 3), trim = TRUE))
            },
            character(1)
          ),
          collapse = "; "
        )
      }
      pieces = c(pieces, paste0("Other numeric predictors fixed at: ", numericText))
    }

    if (length(pieces) == 0) {
      return("No other predictor settings are needed for this quantity.")
    }

    paste(pieces, collapse = ". ")
  }

  addPredictedRow = function(label, newData, settingsText) {

    pred = predict(model, newdata = newData, se.fit = TRUE, type = predType)

    eta = as.numeric(pred$fit)[1]
    seEta = as.numeric(pred$se.fit)[1]

    crit = if (inherits(model, "lm")) {
      stats::qt(1 - (1 - level) / 2, df = model$df.residual)
    } else {
      qnorm(1 - (1 - level) / 2)
    }

    lowerEta = eta - crit * seEta
    upperEta = eta + crit * seEta

    if (is.null(familyObj) || identical(familyObj$link, "identity")) {
      estimate = eta
      lower = lowerEta
      upper = upperEta
      scaleLabel = "response"
      scaleNote = "Computed directly on the response scale."
    } else {
      estimate = familyObj$linkinv(eta)
      lower = familyObj$linkinv(lowerEta)
      upper = familyObj$linkinv(upperEta)
      scaleLabel = getResponseScaleLabel()
      scaleNote = paste(
        "Computed on the", familyObj$link, "scale, then transformed back to the",
        scaleLabel, "scale using the inverse link function."
      )
    }

    mm = model.matrix(delete.response(tt), data = newData)
    xVec = as.numeric(mm[1, ])
    names(xVec) = colnames(mm)

    nonZero = xVec[abs(xVec) > 1e-12]

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
      builtFrom = buildLinearCombinationText(nonZero),
      varianceFormula = buildLinearCombinationVarianceText(nonZero),
      scaleNote = scaleNote
    )
  }

  responseName = names(mf)[1]
  hasFactorPredictor = any(vapply(mf[predictorNames], is.factor, logical(1)))

  baselineSettings = paste(
    vapply(
      predictorNames,
      function(varName) {
        formatBaseSetting(varName, baseRow[[varName]][1])
      },
      character(1)
    ),
    collapse = "; "
  )

  if (!hasFactorPredictor) {
    addPredictedRow(
      label = paste0("Expected ", responseName, " at base settings"),
      newData = baseRow,
      settingsText = baselineSettings
    )
  }

  for (varName in predictorNames) {
    x = mf[[varName]]

    if (is.factor(x)) {
      levs = levels(x)
      for (lvl in levs) {
        newData = baseRow
        newData[[varName]] = factor(lvl, levels = levs)

        settingsText = paste0(
          varName, " = ", lvl, ". ",
          buildOtherBaseLevelsText(excludeVarName = varName)
        )

        addPredictedRow(
          label = paste0("Expected ", responseName, " when ", varName, " = ", lvl),
          newData = newData,
          settingsText = settingsText
        )
      }
    }
  }

  coefVec = coef(model)
  coefNames = names(coefVec)
  vc = vcov(model)

  for (varName in predictorNames) {
    x = mf[[varName]]

    if (!is.numeric(x)) {
      next
    }

    if (!(varName %in% coefNames)) {
      next
    }

    se = sqrt(vc[varName, varName])
    crit = if (inherits(model, "lm")) {
      qt(1 - (1 - level) / 2, df = model$df.residual)
    } else {
      qnorm(1 - (1 - level) / 2)
    }

    est = coefVec[[varName]]
    lower = est - crit * se
    upper = est + crit * se

    if (inherits(model, "glm") && identical(familyObj$family, "binomial") && identical(familyObj$link, "logit")) {
      label = paste0("Odds multiplier for a 1-unit increase in ", varName)
      estimate = exp(est)
      lowerOut = exp(lower)
      upperOut = exp(upper)
      scaleLabel = "odds"
      scaleNote = "Computed from one coefficient on the logit scale, then exponentiated to the odds-multiplier scale."
    } else if (inherits(model, "glm") && identical(familyObj$family, "poisson") && identical(familyObj$link, "log")) {
      label = paste0("Expected-count multiplier for a 1-unit increase in ", varName)
      estimate = exp(est)
      lowerOut = exp(lower)
      upperOut = exp(upper)
      scaleLabel = "multiplier"
      scaleNote = "Computed from one coefficient on the log scale, then exponentiated to the expected-count multiplier scale."
    } else {
      label = paste0("Change in ", responseName, " for a 1-unit increase in ", varName)
      estimate = est
      lowerOut = lower
      upperOut = upper
      scaleLabel = if (is.null(familyObj) || identical(familyObj$link, "identity")) {
        "response"
      } else {
        "link"
      }
      scaleNote = "Computed directly from a single coefficient."
    }

    rows[[length(rows) + 1]] = data.frame(
      quantity = label,
      estimate = round(estimate, 3),
      lower = round(lowerOut, 3),
      upper = round(upperOut, 3),
      scale = scaleLabel,
      stringsAsFactors = FALSE
    )

    details[[length(details) + 1]] = list(
      label = label,
      quantity = label,
      settings = "A 1-unit increase in the named numeric predictor, with no additional row-specific settings.",
      builtFrom = buildLinearCombinationText(setNames(1, varName)),
      varianceFormula = buildLinearCombinationVarianceText(setNames(1, varName)),
      scaleNote = scaleNote
    )
  }

  note = if (length(Filter(function(x) is.factor(x), mf[predictorNames])) > 0) {
    factorBaseText = paste(
      vapply(
        predictorNames[vapply(mf[predictorNames], is.factor, logical(1))],
        function(varName) {
          paste0(varName, " = ", as.character(baseRow[[varName]][1]))
        },
        character(1)
      ),
      collapse = "; "
    )

    numericText = if (length(predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]) > 0) {
      if (identical(numericReference, "zero")) {
        "Numeric predictors are fixed at 0."
      } else {
        "Numeric predictors are fixed at their means."
      }
    } else {
      NULL
    }

    paste(
      "Rows involving factor levels are shown as expected values with other factors held at their base levels.",
      paste0("Base levels: ", factorBaseText, "."),
      numericText
    )
  } else {
    NULL
  }

  list(
    table = do.call(rbind, rows),
    details = details,
    note = note,
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

  crit = if (inherits(model, "lm")) {
    qt(1 - (1 - level) / 2, df = model$df.residual)
  } else {
    qnorm(1 - (1 - level) / 2)
  }

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
