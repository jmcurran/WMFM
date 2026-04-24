#' Build deterministic model-profile metadata for explanations
#'
#' Builds a compact model profile that can be shared by explanation prompts,
#' deterministic formatting, auditing, validation, and developer feedback. The
#' profile describes the fitted model structure without changing the existing
#' explanation-audit or claim-evidence systems.
#'
#' @param model A fitted model object.
#' @param data Optional data frame used to fit the model. If omitted, the model
#'   frame is recovered from `model` where possible.
#' @param modelType Optional WMFM model type, such as `"lm"`, `"logistic"`,
#'   or `"poisson"`.
#'
#' @return A named list with deterministic model-profile metadata.
#'
#' @keywords internal
buildExplanationModelProfile = function(model, data = NULL, modelType = NULL) {

  if (missing(model) || is.null(model)) {
    stop("`model` must be supplied.", call. = FALSE)
  }

  modelFrame = getExplanationModelProfileData(model = model, data = data)
  modelFormula = stats::formula(model)
  modelTerms = stats::terms(model)
  termLabels = attr(modelTerms, "term.labels")

  responseExpr = deparse(modelFormula[[2]])
  responseVariable = getExplanationProfileResponseVariable(
    responseExpr = responseExpr,
    modelFrame = modelFrame
  )

  transformationType = getExplanationProfileTransform(
    responseExpr = responseExpr,
    responseVariable = responseVariable
  )

  modelFamily = getExplanationProfileModelFamily(
    model = model,
    modelType = modelType
  )

  predictorVariables = getExplanationProfilePredictorVariables(
    modelTerms = modelTerms,
    modelFrame = modelFrame,
    responseVariable = responseVariable
  )

  predictorTypes = getExplanationProfilePredictorTypes(
    predictorVariables = predictorVariables,
    modelFrame = modelFrame
  )

  hasInteractions = any(grepl(":", termLabels, fixed = TRUE))
  interactionTypes = getExplanationProfileInteractionTypes(
    termLabels = termLabels,
    predictorTypes = predictorTypes
  )

  modelStructure = getExplanationProfileModelStructure(
    termLabels = termLabels,
    predictorTypes = predictorTypes,
    hasInteractions = hasInteractions
  )

  interpretationScale = getExplanationProfileInterpretationScale(
    modelFamily = modelFamily,
    transformationType = transformationType
  )

  comparisonScope = getExplanationProfileComparisonScope(
    modelStructure = modelStructure,
    predictorTypes = predictorTypes,
    hasInteractions = hasInteractions
  )

  list(
    modelFamily = modelFamily,
    modelStructure = modelStructure,
    modelScale = getExplanationProfileModelScale(
      modelFamily = modelFamily,
      responseExpr = responseExpr,
      transformationType = transformationType
    ),
    interpretationScale = interpretationScale,
    responseVariable = responseVariable,
    responseExpression = responseExpr,
    transformationType = transformationType,
    predictorTypes = predictorTypes,
    hasInteractions = hasInteractions,
    interactionTypes = interactionTypes,
    comparisonScope = comparisonScope,
    requiresAnchor = length(predictorTypes$numeric) > 0,
    sourceFunction = "buildExplanationModelProfile"
  )
}

getExplanationModelProfileData = function(model, data = NULL) {

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("`data` must be a data.frame or NULL.", call. = FALSE)
    }

    return(data)
  }

  out = tryCatch(
    stats::model.frame(model),
    error = function(e) {
      NULL
    }
  )

  if (is.null(out) || !is.data.frame(out)) {
    return(data.frame())
  }

  out
}

getExplanationProfileModelFamily = function(model, modelType = NULL) {

  if (!is.null(modelType) && is.character(modelType) && length(modelType) == 1) {
    modelType = trimws(modelType)

    if (modelType %in% c("lm", "logistic", "poisson")) {
      return(modelType)
    }
  }

  if (isSupportedLogisticModel(model = model)) {
    return("logistic")
  }

  if (isSupportedPoissonModel(model = model)) {
    return("poisson")
  }

  if (inherits(model, "lm")) {
    return("lm")
  }

  "unknown"
}

getExplanationProfileResponseVariable = function(responseExpr, modelFrame) {

  responseVars = all.vars(stats::as.formula(paste("~", responseExpr)))

  if (length(responseVars) > 0) {
    return(responseVars[1])
  }

  if (is.data.frame(modelFrame) && ncol(modelFrame) > 0) {
    return(names(modelFrame)[1])
  }

  NA_character_
}

getExplanationProfileTransform = function(responseExpr, responseVariable) {

  inlineTransform = detectRespTransform(responseExpr)

  if (!is.null(inlineTransform)) {
    return(inlineTransform)
  }

  if (!is.character(responseVariable) || length(responseVariable) != 1 || is.na(responseVariable)) {
    return("none")
  }

  responseVariableLower = tolower(responseVariable)

  if (grepl("^log[._]", responseVariableLower)) {
    return("log")
  }

  if (grepl("^log1p[._]", responseVariableLower)) {
    return("log1p")
  }

  if (grepl("^sqrt[._]", responseVariableLower)) {
    return("sqrt")
  }

  "none"
}

getExplanationProfilePredictorVariables = function(modelTerms, modelFrame, responseVariable) {

  termLabels = attr(modelTerms, "term.labels")

  if (length(termLabels) == 0) {
    return(character(0))
  }

  predictorVariables = unique(unlist(lapply(termLabels, function(termLabel) {
    all.vars(stats::as.formula(paste("~", termLabel)))
  })))
  predictorVariables = predictorVariables[!predictorVariables %in% responseVariable]

  if (is.data.frame(modelFrame) && ncol(modelFrame) > 0) {
    predictorVariables = predictorVariables[predictorVariables %in% names(modelFrame)]
  }

  predictorVariables
}

getExplanationProfilePredictorTypes = function(predictorVariables, modelFrame) {

  out = list(
    numeric = character(0),
    factor = character(0),
    other = character(0)
  )

  if (length(predictorVariables) == 0 || !is.data.frame(modelFrame) || ncol(modelFrame) == 0) {
    return(out)
  }

  for (predictorVariable in predictorVariables) {
    x = modelFrame[[predictorVariable]]

    if (is.numeric(x) || is.integer(x)) {
      out$numeric = c(out$numeric, predictorVariable)
    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      out$factor = c(out$factor, predictorVariable)
    } else {
      out$other = c(out$other, predictorVariable)
    }
  }

  out
}

getExplanationProfileInteractionTypes = function(termLabels, predictorTypes) {

  interactionLabels = termLabels[grepl(":", termLabels, fixed = TRUE)]

  if (length(interactionLabels) == 0) {
    return(character(0))
  }

  vapply(interactionLabels, function(interactionLabel) {
    interactionVars = all.vars(stats::as.formula(paste("~", interactionLabel)))
    numericCount = sum(interactionVars %in% predictorTypes$numeric)
    factorCount = sum(interactionVars %in% predictorTypes$factor)

    if (numericCount >= 2 && factorCount == 0) {
      return("numericByNumeric")
    }

    if (numericCount >= 1 && factorCount >= 1) {
      return("numericByFactor")
    }

    if (factorCount >= 2 && numericCount == 0) {
      return("factorByFactor")
    }

    "mixedOrUnknown"
  }, character(1), USE.NAMES = FALSE) |>
    unique()
}

getExplanationProfileModelStructure = function(termLabels, predictorTypes, hasInteractions) {

  if (length(termLabels) == 0) {
    return("interceptOnly")
  }

  if (isTRUE(hasInteractions)) {
    return("interaction")
  }

  numericCount = length(predictorTypes$numeric)
  factorCount = length(predictorTypes$factor)
  otherCount = length(predictorTypes$other)
  totalCount = numericCount + factorCount + otherCount

  if (totalCount == 1 && numericCount == 1) {
    return("numericMainEffect")
  }

  if (totalCount == 1 && factorCount == 1) {
    return("factorMainEffect")
  }

  "additive"
}

getExplanationProfileModelScale = function(modelFamily, responseExpr, transformationType) {

  if (identical(modelFamily, "logistic")) {
    return("logOdds")
  }

  if (identical(modelFamily, "poisson")) {
    return("logExpectedCount")
  }

  if (!identical(transformationType, "none")) {
    return(responseExpr)
  }

  "response"
}

getExplanationProfileInterpretationScale = function(modelFamily, transformationType) {

  if (identical(modelFamily, "logistic")) {
    return("probability")
  }

  if (identical(modelFamily, "poisson")) {
    return("expectedCount")
  }

  if (!identical(transformationType, "none")) {
    return("originalResponse")
  }

  "response"
}

getExplanationProfileComparisonScope = function(modelStructure, predictorTypes, hasInteractions) {

  if (identical(modelStructure, "interceptOnly")) {
    return("none")
  }

  if (isTRUE(hasInteractions)) {
    return("targeted")
  }

  if (length(predictorTypes$factor) > 0) {
    return("minimal")
  }

  "minimal"
}
