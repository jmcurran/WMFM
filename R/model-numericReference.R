#' Choose the numeric reference used for teaching summaries
#'
#' Selects whether numeric predictors should be anchored at 0 or at their
#' sample means when building teaching-oriented summaries such as fitted-value
#' confidence-interval rows. The current rule is intentionally simple:
#' if every numeric predictor has an observed range that includes 0, use 0;
#' otherwise use the sample mean.
#'
#' @param model Optional fitted model object. Used only when \code{mf}
#'   is not supplied.
#' @param mf Optional model frame. If omitted, it is computed from
#'   \code{model}.
#' @param predictorNames Optional character vector of predictor names.
#'
#' @return A single string, either \code{"zero"} or \code{"mean"}.
#' @keywords internal
#'
#' @importFrom stats model.frame
chooseModelNumericReference = function(model = NULL, mf = NULL, predictorNames = NULL) {

  if (is.null(mf)) {
    if (is.null(model)) {
      stop("Supply either `model` or `mf`.", call. = FALSE)
    }

    mf = model.frame(model)
  }

  if (is.null(predictorNames)) {
    predictorNames = names(mf)[-1]
  }

  if (length(predictorNames) == 0) {
    return("zero")
  }

  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(numericNames) == 0) {
    return("zero")
  }

  zeroInRange = vapply(
    numericNames,
    function(varName) {
      x = mf[[varName]]
      x = x[!is.na(x)]

      if (length(x) == 0) {
        return(TRUE)
      }

      min(x) <= 0 && max(x) >= 0
    },
    logical(1)
  )

  if (isLogisticModelForNumericReference(model)) {
    return("mean")
  }

  if (all(zeroInRange)) {
    return("zero")
  }

  "mean"
}

isLogisticModelForNumericReference = function(model) {

  inherits(model, "glm") &&
    identical(model$family$family, "binomial") &&
    identical(model$family$link, "logit")
}

#' Build numeric-anchor metadata for model prompts and cache keys
#'
#' Summarises the observed range and the chosen teaching anchor for each
#' numeric predictor so prompt builders can avoid interpreting model quantities
#' at meaningless values such as 0 when 0 lies well outside the data.
#'
#' @param model Optional fitted model object. Used only when \code{mf}
#'   is not supplied.
#' @param mf Optional model frame. If omitted, it is computed from
#'   \code{model}.
#' @param predictorNames Optional character vector of predictor names.
#'
#' @return A list with elements \code{numericReference}, \code{promptText},
#'   and \code{cacheKey}.
#' @keywords internal
#'
#' @importFrom stats model.frame
buildModelNumericAnchorInfo = function(model = NULL, mf = NULL, predictorNames = NULL) {

  if (is.null(mf)) {
    if (is.null(model)) {
      stop("Supply either `model` or `mf`.", call. = FALSE)
    }

    mf = model.frame(model)
  }

  if (is.null(predictorNames)) {
    predictorNames = names(mf)[-1]
  }

  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]
  numericReference = chooseModelNumericReference(
    model = model,
    mf = mf,
    predictorNames = predictorNames
  )

  if (length(numericNames) == 0) {
    return(list(
      numericReference = numericReference,
      promptText = paste(
        "Numeric interpretation anchor:",
        "- There are no numeric predictors, so no special numeric anchor is needed."
      ),
      cacheKey = "no_numeric"
    ))
  }

  fmtValue = function(x) {
    format(round(x, 4), trim = TRUE, scientific = FALSE)
  }

  anchorLines = vapply(
    numericNames,
    function(varName) {
      x = mf[[varName]]
      x = x[!is.na(x)]

      if (length(x) == 0) {
        rangeText = "all values missing"
        anchorValue = if (identical(numericReference, "zero")) 0 else NA_real_
        reasonText = if (identical(numericReference, "zero")) {
          "0 is used because there are no observed non-missing values."
        } else {
          "the sample mean would usually be used, but all values are missing."
        }
      } else {
        rangeText = paste0("[", fmtValue(min(x)), ", ", fmtValue(max(x)), "]")
        if (identical(numericReference, "zero")) {
          anchorValue = 0
          reasonText = "0 lies inside the observed range."
        } else {
          anchorValue = mean(x)
          if (isLogisticModelForNumericReference(model)) {
            reasonText = "for logistic explanations, use the sample mean so fitted probabilities are described at a typical value rather than at 0."
          } else {
            reasonText = "0 lies outside the observed range, so use the sample mean instead."
          }
        }
      }

      anchorLabel = if (is.na(anchorValue)) {
        "NA"
      } else {
        fmtValue(anchorValue)
      }

      paste0(
        "- ", varName,
        ": observed range = ", rangeText,
        "; chosen anchor = ", anchorLabel,
        " (", reasonText, ")"
      )
    },
    character(1)
  )

  cacheParts = vapply(
    numericNames,
    function(varName) {
      x = mf[[varName]]
      x = x[!is.na(x)]

      if (length(x) == 0) {
        anchorValue = if (identical(numericReference, "zero")) "0" else "NA"
        rangeText = "NA:NA"
      } else {
        anchorValue = if (identical(numericReference, "zero")) {
          "0"
        } else {
          fmtValue(mean(x))
        }
        rangeText = paste0(fmtValue(min(x)), ":", fmtValue(max(x)))
      }

      paste(varName, rangeText, anchorValue, sep = "=")
    },
    character(1)
  )

  promptText = paste(
    c(
      "Numeric interpretation anchor:",
      anchorLines,
      "Use these anchor values for narrative interpretation of baseline fitted values and comparisons involving numeric predictors.",
      "Do not interpret an intercept or baseline fitted value at 0 when 0 lies outside the observed range.",
      "Formal fitted equations may still be written as functions of the numeric predictor itself."
    ),
    collapse = "\n"
  )

  list(
    numericReference = numericReference,
    promptText = promptText,
    cacheKey = paste(c(numericReference, cacheParts), collapse = "|")
  )
}


#' Build a short UI note about numeric interpretation anchors
#'
#' Returns a concise note for student-facing UI sections when a fitted model
#' has numeric predictors. The note explains whether baseline fitted values are
#' being described at 0 or at sample means.
#'
#' @param model Optional fitted model object. Used only when \code{mf}
#'   is not supplied.
#' @param mf Optional model frame. If omitted, it is computed from
#'   \code{model}.
#' @param predictorNames Optional character vector of predictor names.
#'
#' @return A character scalar. Returns \code{""} when no numeric predictors are
#'   present.
#' @keywords internal
#'
#' @importFrom stats model.frame
buildNumericAnchorUiNote = function(model = NULL, mf = NULL, predictorNames = NULL) {

  if (is.null(mf)) {
    if (is.null(model)) {
      stop("Supply either `model` or `mf`.", call. = FALSE)
    }

    mf = model.frame(model)
  }

  if (is.null(predictorNames)) {
    predictorNames = names(mf)[-1]
  }

  numericNames = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (length(numericNames) == 0) {
    return("")
  }

  numericReference = chooseModelNumericReference(
    model = model,
    mf = mf,
    predictorNames = predictorNames
  )

  fmtValue = function(x) {
    format(round(x, 4), trim = TRUE, scientific = FALSE)
  }

  detailText = vapply(
    numericNames,
    function(varName) {
      x = mf[[varName]]
      x = x[!is.na(x)]

      if (length(x) == 0) {
        if (identical(numericReference, "zero")) {
          return(paste0(varName, " = 0 (all values missing)"))
        }

        return(paste0(varName, " = NA (all values missing)"))
      }

      if (identical(numericReference, "zero")) {
        return(paste0(
          varName,
          " observed range [",
          fmtValue(min(x)),
          ", ",
          fmtValue(max(x)),
          "]"
        ))
      }

      paste0(
        varName,
        " = ",
        fmtValue(mean(x)),
        " (observed range [",
        fmtValue(min(x)),
        ", ",
        fmtValue(max(x)),
        "])"
      )
    },
    character(1)
  )

  if (identical(numericReference, "zero")) {
    return(paste0(
      "For numeric predictors, baseline fitted values are described at 0 because 0 lies inside the observed data range: ",
      paste(detailText, collapse = "; "),
      "."
    ))
  }

  paste0(
    "For numeric predictors, baseline fitted values are described at their sample means unless stated otherwise: ",
    paste(detailText, collapse = "; "),
    "."
  )
}
