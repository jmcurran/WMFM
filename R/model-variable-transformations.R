#' Create derived-variable transformation metadata
#'
#' Records the expression used to create a derived variable without changing how
#' the expression is evaluated. The metadata is intentionally conservative: it
#' preserves the original expression and only labels transformations that WMFM can
#' recognise safely for later explanation and back-transformation work.
#'
#' @param variable Character name of the derived variable.
#' @param rhs Parsed right-hand-side expression used to create the variable.
#' @param data Data frame available when the variable was created.
#' @param expressionText Optional original assignment text entered by the user.
#' @param createdFrom Character label for the source workflow.
#'
#' @return A list with class \code{wmfmVariableTransformation}.
#' @keywords internal
createVariableTransformationRecord = function(
  variable,
  rhs,
  data,
  expressionText = NULL,
  createdFrom = "addDerivedVariable"
) {
  sourceVariables = extractSourceVariables(rhs = rhs, data = data)
  transformationInfo = inferTransformationType(rhs)

  out = list(
    variable = variable,
    expression = expressionText %||% paste0(variable, " = ", deparseOneLine(rhs)),
    rhs = deparseOneLine(rhs),
    sourceVariables = sourceVariables,
    transformationType = transformationInfo$transformationType,
    inverseType = transformationInfo$inverseType,
    transformationParameters = transformationInfo$parameters,
    createdFrom = createdFrom,
    createdAt = Sys.time()
  )

  class(out) = c("wmfmVariableTransformation", "list")
  out
}

#' Extract source variables from a derived-variable expression
#'
#' @param rhs Parsed right-hand-side expression.
#' @param data Data frame available when the expression was created.
#'
#' @return Character vector of data-column names used by \code{rhs}.
#' @keywords internal
extractSourceVariables = function(rhs, data) {
  vars = all.vars(rhs)
  intersect(vars, names(data))
}

#' Infer a conservative transformation label
#'
#' @param rhs Parsed right-hand-side expression.
#'
#' @return A list containing \code{transformationType}, \code{inverseType}, and
#'   \code{parameters}.
#' @keywords internal
inferTransformationType = function(rhs) {
  if (is.call(rhs)) {
    funName = as.character(rhs[[1]])

    if (funName %in% c("log", "log10", "log1p", "sqrt", "factor")) {
      return(list(
        transformationType = funName,
        inverseType = inverseTypeForFunction(funName),
        parameters = list()
      ))
    }

    if (identical(funName, "I") && length(rhs) == 2L) {
      return(inferTransformationType(rhs[[2]]))
    }

    arithmeticInfo = inferArithmeticTransformation(rhs, funName)
    if (!is.null(arithmeticInfo)) {
      return(arithmeticInfo)
    }
  }

  list(
    transformationType = "custom",
    inverseType = "unknown",
    parameters = list()
  )
}

#' Infer a simple arithmetic transformation label
#'
#' @param rhs Parsed right-hand-side expression.
#' @param funName Character arithmetic operator.
#'
#' @return Transformation information or \code{NULL}.
#' @keywords internal
inferArithmeticTransformation = function(rhs, funName) {
  if (!(funName %in% c("+", "-", "*", "/", "^")) || length(rhs) != 3L) {
    return(NULL)
  }

  lhs = rhs[[2]]
  rhsValue = rhs[[3]]

  if (is.name(lhs) && is.numeric(rhsValue) && length(rhsValue) == 1L) {
    return(arithmeticTransformationInfo(funName, constant = rhsValue, variablePosition = "left"))
  }

  if (is.numeric(lhs) && length(lhs) == 1L && is.name(rhsValue)) {
    return(arithmeticTransformationInfo(funName, constant = lhs, variablePosition = "right"))
  }

  NULL
}

#' Build simple arithmetic transformation information
#'
#' @param funName Character arithmetic operator.
#' @param constant Numeric constant.
#' @param variablePosition Character position of the data variable.
#'
#' @return Transformation information.
#' @keywords internal
arithmeticTransformationInfo = function(funName, constant, variablePosition) {
  if (identical(funName, "+")) {
    return(list(
      transformationType = "addConstant",
      inverseType = "subtractConstant",
      parameters = list(constant = constant, variablePosition = variablePosition)
    ))
  }

  if (identical(funName, "-")) {
    inverseType = if (identical(variablePosition, "left")) {
      "addConstant"
    } else {
      "subtractFromConstant"
    }

    return(list(
      transformationType = "subtractConstant",
      inverseType = inverseType,
      parameters = list(constant = constant, variablePosition = variablePosition)
    ))
  }

  if (identical(funName, "*")) {
    return(list(
      transformationType = "multiplyConstant",
      inverseType = "divideConstant",
      parameters = list(constant = constant, variablePosition = variablePosition)
    ))
  }

  if (identical(funName, "/")) {
    inverseType = if (identical(variablePosition, "left")) {
      "multiplyConstant"
    } else {
      "reciprocalScale"
    }

    return(list(
      transformationType = "divideConstant",
      inverseType = inverseType,
      parameters = list(constant = constant, variablePosition = variablePosition)
    ))
  }

  if (identical(funName, "^") && identical(variablePosition, "left")) {
    return(list(
      transformationType = "powerConstant",
      inverseType = "rootConstant",
      parameters = list(constant = constant, variablePosition = variablePosition)
    ))
  }

  NULL
}

#' Get inverse label for a recognised transformation function
#'
#' @param funName Character function name.
#'
#' @return Character inverse label.
#' @keywords internal
inverseTypeForFunction = function(funName) {
  if (identical(funName, "log")) {
    return("exp")
  }

  if (identical(funName, "log10")) {
    return("power10")
  }

  if (identical(funName, "log1p")) {
    return("expm1")
  }

  if (identical(funName, "sqrt")) {
    return("square")
  }

  if (identical(funName, "factor")) {
    return("none")
  }

  "unknown"
}

#' Deparse an expression to one line
#'
#' @param expr Expression to deparse.
#'
#' @return Character scalar.
#' @keywords internal
deparseOneLine = function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}

#' Normalise derived-variable transformation records
#'
#' @param variableTransformations A list of transformation records, or NULL.
#'
#' @return A named list of transformation records.
#' @keywords internal
normaliseVariableTransformations = function(variableTransformations = NULL) {
  if (is.null(variableTransformations)) {
    return(list())
  }

  if (!is.list(variableTransformations)) {
    stop("`variableTransformations` must be a list or NULL.", call. = FALSE)
  }

  if (length(variableTransformations) == 0) {
    return(list())
  }

  recordNames = names(variableTransformations)

  if (is.null(recordNames)) {
    recordNames = rep("", length(variableTransformations))
  }

  out = list()

  for (i in seq_along(variableTransformations)) {
    record = variableTransformations[[i]]

    if (!inherits(record, "wmfmVariableTransformation")) {
      stop("All variable-transformation records must inherit from `wmfmVariableTransformation`.", call. = FALSE)
    }

    recordName = recordNames[[i]]

    if (!nzchar(recordName)) {
      recordName = record$variable %||% ""
    }

    if (!is.character(recordName) || length(recordName) != 1 || !nzchar(recordName)) {
      stop("Each variable-transformation record must have a variable name.", call. = FALSE)
    }

    out[[recordName]] = record
  }

  out
}

#' Select transformation records used by a model formula
#'
#' @param formula A model formula.
#' @param variableTransformations A list of transformation records, or NULL.
#'
#' @return A named list of transformation records referenced by the formula.
#' @keywords internal
getFormulaVariableTransformations = function(formula, variableTransformations = NULL) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must inherit from `formula`.", call. = FALSE)
  }

  records = normaliseVariableTransformations(variableTransformations)

  if (length(records) == 0) {
    return(list())
  }

  formulaVariables = all.vars(formula)
  usedNames = intersect(names(records), formulaVariables)
  records[usedNames]
}

#' Attach derived-variable transformation metadata to a fitted model
#'
#' @param model A fitted model object.
#' @param formula A model formula.
#' @param variableTransformations A list of transformation records, or NULL.
#'
#' @return The fitted model with a `wmfm_variable_transformations` attribute.
#' @keywords internal
attachVariableTransformationsToModel = function(
  model,
  formula,
  variableTransformations = NULL
) {
  attr(model, "wmfm_variable_transformations") = getFormulaVariableTransformations(
    formula = formula,
    variableTransformations = variableTransformations
  )

  model
}

#' Get derived-variable transformation metadata from a fitted model
#'
#' @param model A fitted model object.
#'
#' @return A named list of transformation records.
#' @keywords internal
getModelVariableTransformations = function(model) {
  records = attr(model, "wmfm_variable_transformations", exact = TRUE)
  normaliseVariableTransformations(records)
}

#' Build explanation-audit rows for derived-variable transformations
#'
#' Converts fitted-model transformation records into a compact data frame for the
#' explanation audit. This is intentionally descriptive only; it does not perform
#' numerical back-transformation.
#'
#' @param model A fitted model object.
#'
#' @return A data frame with one row per fitted derived-variable transformation.
#' @keywords internal
buildModelExplanationAuditVariableTransformations = function(model) {
  records = getModelVariableTransformations(model)

  if (length(records) == 0) {
    return(emptyVariableTransformationAuditTable())
  }

  rows = lapply(records, variableTransformationAuditRow)
  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}

#' Build an empty variable-transformation audit table
#'
#' @return A zero-row data frame with the stable audit columns.
#' @keywords internal
emptyVariableTransformationAuditTable = function() {
  data.frame(
    variable = character(0),
    sourceVariables = character(0),
    expression = character(0),
    rhs = character(0),
    transformationType = character(0),
    inverseType = character(0),
    transformationParameters = character(0),
    stringsAsFactors = FALSE
  )
}

#' Convert one variable-transformation record to an audit row
#'
#' @param record A `wmfmVariableTransformation` record.
#'
#' @return A one-row data frame.
#' @keywords internal
variableTransformationAuditRow = function(record) {
  data.frame(
    variable = record$variable %||% "",
    sourceVariables = paste(record$sourceVariables %||% character(0), collapse = ", "),
    expression = record$expression %||% "",
    rhs = record$rhs %||% "",
    transformationType = record$transformationType %||% "custom",
    inverseType = record$inverseType %||% "unknown",
    transformationParameters = formatVariableTransformationParameters(
      record$transformationParameters %||% list()
    ),
    stringsAsFactors = FALSE
  )
}

#' Format variable-transformation parameters for audit display
#'
#' @param parameters A named list of transformation parameters.
#'
#' @return Character scalar.
#' @keywords internal
formatVariableTransformationParameters = function(parameters = list()) {
  if (length(parameters) == 0) {
    return("")
  }

  parameterNames = names(parameters)

  if (is.null(parameterNames)) {
    parameterNames = rep("", length(parameters))
  }

  parts = character(0)

  for (i in seq_along(parameters)) {
    value = parameters[[i]]
    valueText = paste(as.character(value), collapse = ", ")
    nameText = parameterNames[[i]]

    if (nzchar(nameText)) {
      parts = c(parts, paste0(nameText, " = ", valueText))
    } else {
      parts = c(parts, valueText)
    }
  }

  paste(parts, collapse = "; ")
}

#' Build a prompt block describing fitted derived-variable transformations
#'
#' @param variableTransformations A variable-transformation audit table.
#'
#' @return Character scalar prompt block.
#' @keywords internal
buildVariableTransformationPromptBlock = function(variableTransformations = NULL) {
  if (!is.data.frame(variableTransformations) || nrow(variableTransformations) == 0) {
    return(paste(
      "User-created derived variables used by this fitted model: none recorded.",
      "Do not invent back-transformations that are not supported by the fitted model metadata."
    ))
  }

  lines = c(
    "User-created derived variables used by this fitted model:",
    "Use this metadata only as descriptive support. Do not perform automatic back-transformation unless a later deterministic workflow provides it."
  )

  for (i in seq_len(nrow(variableTransformations))) {
    row = variableTransformations[i, , drop = FALSE]
    parameterText = row$transformationParameters

    details = paste0(
      "- ", row$variable,
      ": ", row$rhs,
      "; source variables: ", row$sourceVariables,
      "; transformation: ", row$transformationType,
      "; inverse label: ", row$inverseType
    )

    if (nzchar(parameterText)) {
      details = paste0(details, "; parameters: ", parameterText)
    }

    lines = c(lines, details)
  }

  paste(lines, collapse = "\n")
}
