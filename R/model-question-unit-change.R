#' Add deterministic unit-change interpretation to a follow-up payload
#'
#' @param model Fitted model object.
#' @param followupPayload List returned by \\code{classifyModelFollowupQuestion()}.
#'
#' @return Updated payload list. Adds \\code{unitChangeResult} for supported
#'   unit-change requests.
#' @keywords internal
#' @noRd
enrichFollowupPayloadWithUnitChange = function(model, followupPayload) {
  payload = followupPayload
  if (!is.list(payload) || !identical(payload$category, "unit_change_request")) {
    return(payload)
  }

  unitChange = payload$unitChangeValues %||% numeric(0)
  payload$unitChangeResult = computeModelQuestionUnitChange(
    model = model,
    followupQuestion = payload$originalText %||% "",
    requestedUnitChange = unitChange
  )
  payload$requiresDeterministicComputation = TRUE
  payload
}

#' @keywords internal
#' @noRd
computeModelQuestionUnitChange = function(model, followupQuestion, requestedUnitChange = numeric(0)) {
  if (!inherits(model, "lm")) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_type",
      modelType = class(model)[[1]] %||% "unknown",
      effectScale = "not_available",
      warnings = "Unit-change follow-ups currently require lm or glm model objects."
    ))
  }

  unitChange = resolveRequestedUnitChange(requestedUnitChange = requestedUnitChange)
  modelType = if (inherits(model, "glm")) {
    "glm"
  } else {
    "lm"
  }
  defaultScale = if (identical(modelType, "glm")) {
    "not_available"
  } else {
    "response_difference"
  }

  if (!is.finite(unitChange) || unitChange <= 0) {
    return(list(
      status = "needs_input",
      reason = "missing_or_invalid_unit_change",
      modelType = modelType,
      effectScale = defaultScale,
      warnings = "Provide one positive numeric unit-change size, such as a 0.1-unit or 5-unit increase."
    ))
  }

  mf = stats::model.frame(model)
  responseName = names(mf)[[1]]
  predictorNames = names(mf)[-1]
  numericPredictors = predictorNames[vapply(mf[predictorNames], is.numeric, logical(1))]

  if (!length(numericPredictors)) {
    return(list(
      status = "unsupported",
      reason = "no_numeric_predictors",
      modelType = modelType,
      effectScale = defaultScale,
      warnings = "The fitted model does not contain a numeric predictor that can be re-expressed as a unit-change effect."
    ))
  }

  predictorResolution = resolveUnitChangePredictor(
    followupQuestion = followupQuestion,
    numericPredictors = numericPredictors
  )
  if (!isTRUE(predictorResolution$ok)) {
    return(c(
      list(
        status = predictorResolution$status,
        reason = predictorResolution$reason,
        modelType = modelType,
        effectScale = defaultScale,
        requestedUnitChange = unitChange,
        candidatePredictors = numericPredictors
      ),
      predictorResolution[c("warnings")]
    ))
  }

  predictorName = predictorResolution$predictorName
  coefValues = stats::coef(model)
  if (!(predictorName %in% names(coefValues))) {
    return(list(
      status = "unsupported",
      reason = "unsupported_model_structure",
      modelType = modelType,
      effectScale = defaultScale,
      requestedUnitChange = unitChange,
      predictorName = predictorName,
      warnings = "The requested predictor does not have a simple one-coefficient effect in the fitted model. Interaction, polynomial, transformed, or contrast-coded terms require a later deterministic pathway."
    ))
  }

  oneUnitEffect = unname(as.numeric(coefValues[[predictorName]]))
  ciResult = tryCatch(
    stats::confint(model, parm = predictorName),
    error = function(e) {
      NULL
    }
  )

  if (inherits(model, "glm")) {
    return(computeGlmUnitChangeResult(
      model = model,
      responseName = responseName,
      predictorName = predictorName,
      unitChange = unitChange,
      oneUnitEffect = oneUnitEffect,
      ciResult = ciResult
    ))
  }

  confidenceInterval = NULL
  if (!is.null(ciResult)) {
    confidenceInterval = list(
      level = 0.95,
      oneUnitLwr = unname(as.numeric(ciResult[1, 1])),
      oneUnitUpr = unname(as.numeric(ciResult[1, 2])),
      lwr = unname(as.numeric(ciResult[1, 1])) * unitChange,
      upr = unname(as.numeric(ciResult[1, 2])) * unitChange
    )
  }

  transformedEstimate = oneUnitEffect * unitChange
  list(
    status = "ok",
    modelType = "lm",
    effectScale = "response_difference",
    responseName = responseName,
    predictorName = predictorName,
    requestedUnitChange = unitChange,
    oneUnitEffect = oneUnitEffect,
    transformedEstimate = transformedEstimate,
    unitChangeEffect = transformedEstimate,
    confidenceInterval = confidenceInterval,
    interpretation = paste0(
      "For a ", signif(unitChange, 6), "-unit increase in ", predictorName,
      ", the fitted mean ", responseName, " changes by ",
      signif(transformedEstimate, 6), "."
    )
  )
}

#' @keywords internal
#' @noRd
computeGlmUnitChangeResult = function(model, responseName, predictorName, unitChange, oneUnitEffect, ciResult = NULL) {
  familyName = model$family$family %||% ""
  linkName = model$family$link %||% ""

  if (identical(familyName, "poisson") && identical(linkName, "log")) {
    effectScale = "expected_count_multiplier"
    interpretationNoun = "expected count"
  } else if (identical(familyName, "binomial") && identical(linkName, "logit")) {
    effectScale = "odds_multiplier"
    interpretationNoun = "odds"
  } else {
    return(list(
      status = "unsupported",
      reason = "unsupported_glm_family_link",
      modelType = "glm",
      glmFamily = familyName,
      glmLink = linkName,
      effectScale = "not_available",
      responseName = responseName,
      predictorName = predictorName,
      requestedUnitChange = unitChange,
      warnings = "Only Poisson log-link and binomial logit-link unit-change effects are currently supported."
    ))
  }

  linkScaleEffect = oneUnitEffect * unitChange
  transformedEstimate = exp(linkScaleEffect)
  confidenceInterval = NULL
  if (!is.null(ciResult)) {
    ciValues = as.numeric(ciResult)
    if (length(ciValues) >= 2L && all(is.finite(ciValues[1:2]))) {
      oneUnitLwr = unname(ciValues[[1]])
      oneUnitUpr = unname(ciValues[[2]])
      confidenceInterval = list(
        level = 0.95,
        oneUnitLwr = oneUnitLwr,
        oneUnitUpr = oneUnitUpr,
        lwr = exp(oneUnitLwr * unitChange),
        upr = exp(oneUnitUpr * unitChange)
      )
    }
  }

  list(
    status = "ok",
    modelType = "glm",
    glmFamily = familyName,
    glmLink = linkName,
    effectScale = effectScale,
    responseName = responseName,
    predictorName = predictorName,
    requestedUnitChange = unitChange,
    oneUnitEffect = oneUnitEffect,
    linkScaleUnitChangeEffect = linkScaleEffect,
    transformedEstimate = transformedEstimate,
    unitChangeEffect = transformedEstimate,
    confidenceInterval = confidenceInterval,
    interpretation = paste0(
      "For a ", signif(unitChange, 6), "-unit increase in ", predictorName,
      ", the ", interpretationNoun, " is multiplied by ",
      signif(transformedEstimate, 6), "."
    )
  )
}

#' @keywords internal
#' @noRd
resolveRequestedUnitChange = function(requestedUnitChange = numeric(0)) {
  values = suppressWarnings(as.numeric(requestedUnitChange %||% numeric(0)))
  values = values[is.finite(values)]
  values = unique(values)
  if (length(values) == 0) {
    return(NA_real_)
  }
  if (length(values) > 1) {
    return(NA_real_)
  }
  values[[1]]
}

#' @keywords internal
#' @noRd
resolveUnitChangePredictor = function(followupQuestion, numericPredictors) {
  text = tolower(trimws(as.character(followupQuestion %||% "")))
  if (!nzchar(text)) {
    text = ""
  }

  matches = vapply(numericPredictors, function(predictorName) {
    escapedName = escapeRegexLiteral(tolower(predictorName))
    grepl(paste0("(?<![a-z0-9_.])", escapedName, "(?![a-z0-9_.])"), text, perl = TRUE)
  }, logical(1))

  matchedPredictors = numericPredictors[matches]
  if (length(matchedPredictors) == 1L) {
    return(list(ok = TRUE, predictorName = matchedPredictors[[1]]))
  }

  if (length(matchedPredictors) > 1L) {
    return(list(
      ok = FALSE,
      status = "clarification_required",
      reason = "ambiguous_predictor",
      warnings = paste0(
        "The unit-change request mentions multiple numeric predictors: ",
        paste(matchedPredictors, collapse = ", "),
        ". Please name exactly one predictor."
      )
    ))
  }

  if (length(numericPredictors) == 1L) {
    return(list(ok = TRUE, predictorName = numericPredictors[[1]]))
  }

  list(
    ok = FALSE,
    status = "clarification_required",
    reason = "ambiguous_predictor",
    warnings = paste0(
      "The fitted model has multiple numeric predictors: ",
      paste(numericPredictors, collapse = ", "),
      ". Please name the predictor for the unit-change interpretation."
    )
  )
}

#' @keywords internal
#' @noRd
escapeRegexLiteral = function(x) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x, perl = TRUE)
}
