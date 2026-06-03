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
