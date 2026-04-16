#' Render one deterministic equation case
#'
#' Converts one deterministic equation specification and one teaching case into
#' rendered equation strings. The output separates the linear predictor from any
#' transformed scales so later stages can decide how to present them.
#'
#' Supported first-pass families are Gaussian identity, binomial logit, and
#' Poisson log models under the package's usual treatment-coding assumptions.
#'
#' @param spec A deterministic equation specification from
#'   \code{buildEquationSpec()}.
#' @param case One equation case from \code{buildEquationCases()}.
#' @param digits Integer number of decimal places for displayed coefficients.
#'   Defaults to 2.
#'
#' @return A list of class \code{"wmfmEquationRenderCase"}.
#'
#' @keywords internal
renderEquationCase = function(spec, case, digits = 2) {

  if (!inherits(spec, "wmfmEquationSpec")) {
    stop("`spec` must inherit from `wmfmEquationSpec`.", call. = FALSE)
  }

  requiredCaseFields = c("caseId", "label", "factorValues", "retainedNumeric")

  if (!is.list(case) || !all(requiredCaseFields %in% names(case))) {
    stop("`case` must be a case object produced by `buildEquationCases()`.", call. = FALSE)
  }

  coefficientMap = as.numeric(spec$coefficients$estimate)
  names(coefficientMap) = spec$coefficients$term

  predictorInfo = spec$predictors
  factorNames = names(predictorInfo)[vapply(predictorInfo, function(x) x$type, character(1)) == "factor"]
  numericNames = case$retainedNumeric

  constantComponents = numeric(0)
  slopeComponents = vector("list", length(numericNames))
  names(slopeComponents) = numericNames
  otherComponents = list()

  for (termName in names(coefficientMap)) {
    estimate = coefficientMap[[termName]]

    if (identical(termName, "(Intercept)")) {
      constantComponents = c(constantComponents, estimate)
      next
    }

    pieces = strsplit(termName, ":", fixed = TRUE)[[1]]
    active = TRUE
    numericPieces = character(0)

    for (piece in pieces) {
      if (piece %in% spec$predictorNames) {
        numericPieces = c(numericPieces, piece)
        next
      }

      matchedFactor = FALSE

      for (factorName in factorNames) {
        levels = predictorInfo[[factorName]]$levels
        nonReferenceLevels = levels[levels != predictorInfo[[factorName]]$reference]

        for (level in nonReferenceLevels) {
          dummyName = paste0(factorName, level)

          if (identical(piece, dummyName)) {
            matchedFactor = TRUE

            if (!identical(case$factorValues[[factorName]], level)) {
              active = FALSE
            }

            break
          }
        }

        if (matchedFactor) {
          break
        }
      }

      if (!matchedFactor && !(piece %in% spec$predictorNames)) {
        active = FALSE
      }

      if (!active) {
        break
      }
    }

    if (!active) {
      next
    }

    if (length(numericPieces) == 0) {
      constantComponents = c(constantComponents, estimate)
    } else if (length(numericPieces) == 1) {
      slopeComponents[[numericPieces]] = c(slopeComponents[[numericPieces]], estimate)
    } else {
      termLabel = paste(numericPieces, collapse = " * ")
      otherComponents[[termLabel]] = c(otherComponents[[termLabel]], estimate)
    }
  }

  unsimplifiedPieces = character(0)
  simplifiedPieces = character(0)

  if (length(constantComponents) > 0) {
    unsimplifiedPieces = c(
      unsimplifiedPieces,
      buildEquationComponentText(constantComponents, digits = digits)
    )
    simplifiedPieces = c(
      simplifiedPieces,
      buildEquationComponentText(sum(constantComponents), digits = digits)
    )
  }

  if (length(numericNames) > 0) {
    for (numericName in numericNames) {
      values = slopeComponents[[numericName]]

      if (length(values) == 0) {
        next
      }

      unsimplifiedPieces = c(
        unsimplifiedPieces,
        buildEquationComponentText(values, variable = numericName, digits = digits)
      )
      simplifiedPieces = c(
        simplifiedPieces,
        buildEquationComponentText(sum(values), variable = numericName, digits = digits)
      )
    }
  }

  if (length(otherComponents) > 0) {
    for (termLabel in names(otherComponents)) {
      values = otherComponents[[termLabel]]
      unsimplifiedPieces = c(
        unsimplifiedPieces,
        buildEquationComponentText(values, variable = termLabel, digits = digits)
      )
      simplifiedPieces = c(
        simplifiedPieces,
        buildEquationComponentText(sum(values), variable = termLabel, digits = digits)
      )
    }
  }

  unsimplifiedRhs = joinEquationPieces(unsimplifiedPieces)
  simplifiedRhs = joinEquationPieces(simplifiedPieces)
  rhs = if (!identical(unsimplifiedRhs, simplifiedRhs)) {
    paste0(unsimplifiedRhs, " = ", simplifiedRhs)
  } else {
    simplifiedRhs
  }

  conditionSuffix = if (case$caseId %in% c("overall", "general")) {
    ""
  } else {
    paste0(" (when ", case$label, ")")
  }

  lhsLinear = if (identical(spec$family, "binomial") && identical(spec$link, "logit")) {
    spec$notation$logitSuccess
  } else if (identical(spec$family, "poisson") && identical(spec$link, "log")) {
    spec$notation$logMean
  } else {
    spec$responseName
  }

  linearPredictor = paste0(lhsLinear, " = ", rhs, conditionSuffix)

  oddsScale = NULL
  responseScale = NULL

  if (identical(spec$family, "binomial") && identical(spec$link, "logit")) {
    oddsScale = paste0(
      spec$notation$oddsSuccess,
      " = exp(", simplifiedRhs, ")",
      conditionSuffix
    )
    responseScale = paste0(
      spec$notation$probabilitySuccess,
      " = exp(", simplifiedRhs, ") / (1 + exp(", simplifiedRhs, "))",
      conditionSuffix
    )
  } else if (identical(spec$family, "poisson") && identical(spec$link, "log")) {
    responseScale = paste0(
      spec$notation$mean,
      " = exp(", simplifiedRhs, ")",
      conditionSuffix
    )
  }

  out = list(
    caseId = case$caseId,
    label = case$label,
    linearPredictor = linearPredictor,
    oddsScale = oddsScale,
    responseScale = responseScale,
    unsimplifiedRhs = unsimplifiedRhs,
    simplifiedRhs = simplifiedRhs
  )

  class(out) = c("wmfmEquationRenderCase", class(out))
  out
}


#' Render a set of deterministic equation cases
#'
#' Applies \code{renderEquationCase()} to a whole set of deterministic teaching
#' cases.
#'
#' @param spec A deterministic equation specification from
#'   \code{buildEquationSpec()}.
#' @param cases Optional equation cases from \code{buildEquationCases()}. When
#'   omitted, the cases are built automatically from \code{spec}.
#' @param digits Integer number of decimal places for displayed coefficients.
#'   Defaults to 2.
#'
#' @return A list of class \code{"wmfmEquationRender"}.
#'
#' @keywords internal
renderEquationCases = function(spec, cases = NULL, digits = 2) {

  if (is.null(cases)) {
    cases = buildEquationCases(spec)
  }

  out = lapply(
    cases,
    function(case) {
      renderEquationCase(spec = spec, case = case, digits = digits)
    }
  )

  class(out) = c("wmfmEquationRender", class(out))
  out
}
