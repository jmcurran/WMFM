#' Detect log-log metadata for a fitted linear model
#'
#' Detects the common teaching model form `log(y) ~ log(x)`, optionally with
#' additional additive adjustment terms. The returned metadata is intentionally
#' conservative and only records a log-log structure when the response is a
#' natural-log transform and at least one predictor term is also a natural-log
#' transform.
#'
#' @param model A fitted model object.
#' @param modelFrame Optional model frame recovered from the fitted model.
#'
#' @return A named list containing log-log metadata.
#'
#' @keywords internal
getLogLogModelMetadata = function(model, modelFrame = NULL) {

  empty = list(
    isLogLog = FALSE,
    responseVariable = NA_character_,
    responseExpression = NA_character_,
    logPredictors = data.frame(
      transformedName = character(0),
      originalName = character(0),
      termLabel = character(0),
      stringsAsFactors = FALSE
    ),
    interpretation = "notLogLog"
  )

  if (missing(model) || is.null(model) || !inherits(model, "lm") || inherits(model, "glm")) {
    return(empty)
  }

  modelFormula = stats::formula(model)
  responseExpression = paste(deparse(modelFormula[[2]]), collapse = "")
  responseLog = parseNaturalLogCall(responseExpression)

  if (is.null(responseLog)) {
    return(empty)
  }

  termLabels = attr(stats::terms(model), "term.labels")

  if (length(termLabels) == 0) {
    return(empty)
  }

  logPredictorInfo = lapply(termLabels, function(termLabel) {
    parsed = parseNaturalLogCall(termLabel)

    if (is.null(parsed)) {
      return(NULL)
    }

    transformedName = termLabel

    if (!is.null(modelFrame) && is.data.frame(modelFrame)) {
      matchingName = names(modelFrame)[names(modelFrame) == termLabel]
      if (length(matchingName) == 1) {
        transformedName = matchingName
      }
    }

    data.frame(
      transformedName = transformedName,
      originalName = parsed,
      termLabel = termLabel,
      stringsAsFactors = FALSE
    )
  })

  logPredictorInfo = Filter(Negate(is.null), logPredictorInfo)

  if (length(logPredictorInfo) == 0) {
    return(empty)
  }

  logPredictors = do.call(rbind, logPredictorInfo)

  list(
    isLogLog = TRUE,
    responseVariable = responseLog,
    responseExpression = responseExpression,
    logPredictors = logPredictors,
    interpretation = "proportionalChange"
  )
}

#' Parse a natural-log call expression
#'
#' @param expr Character expression such as `log(price)`.
#'
#' @return The variable inside the log call, or `NULL` if the expression is not
#'   a simple natural-log call.
#'
#' @keywords internal
parseNaturalLogCall = function(expr) {

  if (is.null(expr) || length(expr) != 1 || is.na(expr)) {
    return(NULL)
  }

  exprNoSpace = gsub("\\s+", "", as.character(expr))
  parsed = tryCatch(str2lang(exprNoSpace), error = function(e) NULL)

  if (is.null(parsed) || !is.call(parsed) || length(parsed) != 2) {
    return(NULL)
  }

  callName = as.character(parsed[[1]])

  if (!identical(callName, "log")) {
    return(NULL)
  }

  inner = parsed[[2]]

  if (!is.symbol(inner)) {
    return(NULL)
  }

  as.character(inner)
}

#' Match a transformed log predictor name to its original variable name
#'
#' @param spec A deterministic equation specification.
#' @param transformedName Character name used in the model matrix or model
#'   frame.
#'
#' @return Original variable name when available, otherwise `NULL`.
#' @keywords internal
getLogLogOriginalPredictorName = function(spec, transformedName) {

  if (is.null(spec$logLog) || !isTRUE(spec$logLog$isLogLog)) {
    return(NULL)
  }

  logPredictors = spec$logLog$logPredictors

  if (!is.data.frame(logPredictors) || nrow(logPredictors) == 0) {
    return(NULL)
  }

  matched = logPredictors$originalName[
    logPredictors$transformedName == transformedName |
      logPredictors$termLabel == transformedName
  ]

  if (length(matched) == 0) {
    return(NULL)
  }

  matched[[1]]
}
