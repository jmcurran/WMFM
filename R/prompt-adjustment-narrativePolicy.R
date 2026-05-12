#' Build explanation narrative policy metadata for adjustment workflows
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A list describing allowed and forbidden narrative axes.
#' @keywords internal
getExplanationNarrativePolicy = function(model, mf = NULL) {
  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)
  adjustmentVariables = roleMetadata$adjustmentPredictors %||% character(0)
  adjustmentVariables = unique(as.character(adjustmentVariables))
  adjustmentVariables = adjustmentVariables[nzchar(adjustmentVariables)]

  list(
    primaryVariables = roleMetadata$primaryPredictors %||% character(0),
    adjustmentVariables = adjustmentVariables,
    hasAdjustments = length(adjustmentVariables) > 0,
    forbiddenNarrativeAxes = getForbiddenNarrativeAxes(model = model, mf = mf),
    allowedFindings = getAllowedExplanationFindings(model = model, mf = mf)
  )
}

#' Get forbidden narrative axes for explanation payload filtering
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return Character vector of forbidden axis labels and levels.
#' @keywords internal
getForbiddenNarrativeAxes = function(model, mf = NULL) {
  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)
  adjustmentVariables = roleMetadata$adjustmentPredictors %||% character(0)
  if (length(adjustmentVariables) == 0) {
    return(character(0))
  }

  if (is.null(mf)) {
    mf = tryCatch(
      stats::model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  forbidden = adjustmentVariables

  for (variableName in adjustmentVariables) {
    if (!is.data.frame(mf) || !variableName %in% names(mf)) {
      next
    }

    x = mf[[variableName]]
    levelLabels = unique(as.character(if (is.factor(x)) levels(x) else x))
    levelLabels = trimws(levelLabels)
    levelLabels = levelLabels[nzchar(levelLabels)]
    forbidden = c(forbidden, levelLabels)
  }

  unique(forbidden)
}

#' Get explanation findings that remain narratively interpretable
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return Character vector of allowed finding variables.
#' @keywords internal
getAllowedExplanationFindings = function(model, mf = NULL) {
  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)
  unique(as.character(roleMetadata$primaryPredictors %||% character(0)))
}

#' Filter explanation payload rows for adjustment-variable narrative safety
#'
#' @param payloadTable Data frame payload intended for LLM interpretation.
#' @param model A fitted model object.
#' @param labelColumns Character vector of payload columns to inspect.
#'
#' @return A filtered data frame that excludes adjustment-related narrative axes.
#' @keywords internal
filterExplanationPayloadForAdjustmentVariables = function(
    payloadTable,
    model,
    labelColumns = c("quantity", "label", "term", "comparison", "group")) {
  if (!is.data.frame(payloadTable) || nrow(payloadTable) == 0) {
    return(payloadTable)
  }

  policy = getExplanationNarrativePolicy(model = model)
  if (!isTRUE(policy$hasAdjustments)) {
    return(payloadTable)
  }

  labelColumns = intersect(labelColumns, names(payloadTable))
  if (length(labelColumns) == 0) {
    return(payloadTable)
  }

  forbiddenAxes = tolower(unique(as.character(policy$forbiddenNarrativeAxes %||% character(0))))
  forbiddenAxes = forbiddenAxes[nzchar(forbiddenAxes)]

  if (length(forbiddenAxes) == 0) {
    return(payloadTable)
  }

  keepRows = rep(TRUE, nrow(payloadTable))
  for (i in seq_len(nrow(payloadTable))) {
    rowValues = tolower(unlist(lapply(labelColumns, function(columnName) {
      as.character(payloadTable[[columnName]][[i]] %||% "")
    }), use.names = FALSE))

    if (any(vapply(rowValues, function(value) {
      any(vapply(forbiddenAxes, function(axis) {
        grepl(axis, value, fixed = TRUE)
      }, logical(1)))
    }, logical(1)))) {
      keepRows[[i]] = FALSE
    }
  }

  payloadTable[keepRows, , drop = FALSE]
}
