#' Build interpretable confidence-interval output for a fitted model
#'
#' Creates a compact table of confidence intervals for quantities that are
#' easier for students to interpret than a raw coefficient table. For models
#' without interaction terms, the helper builds intervals for expected values
#' at simple reference settings and for one-unit changes in numeric predictors.
#' It also records short derivation notes showing when a confidence interval is
#' based on a linear combination of coefficients and therefore uses covariance
#' terms.
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
#'
#' @return A list with components:
#' \describe{
#'   \item{table}{A data frame ready for display in the confidence-interval tab.}
#'   \item{details}{A list of derivation records for optional display.}
#'   \item{note}{Optional explanatory note for the UI.}
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

  if (hasInteractions) {
    out = buildCoefficientOnlyConfidenceIntervalData(model = model, level = level)
    out$note = paste(
      "This model contains interaction terms, so simple one-row teaching summaries",
      "depend on the values chosen for the other predictors.",
      "The table below therefore shows coefficient intervals rather than derived expected values."
    )
    return(out)
  }

  predictorNames = names(mf)[-1]
  if (length(predictorNames) == 0) {
    out = buildCoefficientOnlyConfidenceIntervalData(model = model, level = level)
    out$note = "This model has no predictors, so the confidence-interval table shows the intercept only."
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

  addPredictedRow = function(label, newData, contextText) {

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
    } else {
      estimate = familyObj$linkinv(eta)
      lower = familyObj$linkinv(lowerEta)
      upper = familyObj$linkinv(upperEta)
      scaleLabel = "response"
    }

    mm = model.matrix(delete.response(tt), data = newData)
    xVec = as.numeric(mm[1, ])
    names(xVec) = colnames(mm)

    nonZero = xVec[abs(xVec) > 1e-12]
    varianceExpr = buildLinearCombinationVarianceText(nonZero)

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
      context = contextText,
      combination = buildLinearCombinationText(nonZero),
      variance = varianceExpr,
      note = if (length(nonZero) > 1) {
        paste(
          "Because more than one coefficient contributes to this quantity,",
          "the standard error depends on both the coefficient variances and the covariance terms."
        )
      } else {
        "This quantity comes from a single coefficient, so its standard error comes directly from that coefficient variance."
      }
    )
  }

  responseName = names(mf)[1]

  baselineContext = paste(
    "Reference settings:",
    paste(vapply(
      predictorNames,
      function(varName) {
        value = baseRow[[varName]][1]
        if (is.factor(mf[[varName]])) {
          paste0(varName, " = ", as.character(value))
        } else if (is.numeric(mf[[varName]]) && identical(numericReference, "mean")) {
          paste0(varName, " = mean(", varName, ") = ", format(round(as.numeric(value), 3), trim = TRUE))
        } else {
          paste0(varName, " = ", format(round(as.numeric(value), 3), trim = TRUE))
        }
      },
      character(1)
    ), collapse = "; ")
  )

  addPredictedRow(
    label = paste0("Expected ", responseName, " at reference settings"),
    newData = baseRow,
    contextText = baselineContext
  )

  for (varName in predictorNames) {
    x = mf[[varName]]

    if (is.factor(x)) {
      levs = levels(x)
      for (lvl in levs) {
        newData = baseRow
        newData[[varName]] = factor(lvl, levels = levs)

        contextText = paste0(
          varName, " = ", lvl,
          "; all other factors at their reference levels",
          if (any(vapply(mf[predictorNames], is.numeric, logical(1)))) {
            "; numeric predictors held at their means"
          } else {
            ""
          }
        )

        addPredictedRow(
          label = paste0("Expected ", responseName, " when ", varName, " = ", lvl),
          newData = newData,
          contextText = contextText
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
    } else if (inherits(model, "glm") && identical(familyObj$family, "poisson") && identical(familyObj$link, "log")) {
      label = paste0("Expected-count multiplier for a 1-unit increase in ", varName)
      estimate = exp(est)
      lowerOut = exp(lower)
      upperOut = exp(upper)
      scaleLabel = "multiplier"
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
      context = "Single-coefficient effect summary.",
      combination = paste0(varName, " coefficient only"),
      variance = paste0("Var = Var(", varName, ")"),
      note = "This interval comes directly from one coefficient, so no covariance combination is needed."
    )
  }

  note = if (length(Filter(function(x) is.factor(x), mf[predictorNames])) > 0) {
    paste(
      "Rows involving factor levels are expressed as expected values at simple reference settings.",
      "These rows can combine multiple coefficients, so their standard errors use both variances and covariance terms."
    )
  } else {
    NULL
  }

  list(
    table = do.call(rbind, rows),
    details = details,
    note = note,
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
      context = "Coefficient interval.",
      combination = paste0(name, " coefficient only"),
      variance = paste0("Var = Var(", name, ")"),
      note = "This interval comes directly from one coefficient."
    )
  })

  list(
    table = out,
    details = details,
    note = NULL,
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
    return(paste0("Var = Var(", namesClean[1], ")"))
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
