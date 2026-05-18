#' Return adjustment variables stored on a fitted model
#'
#' @param model A fitted model object.
#'
#' @return Character vector of adjustment-variable names.
#'
#' @keywords internal
getModelAdjustmentVariables = function(model) {
  adjustmentVariables = attr(model, "wmfm_adjustment_variables", exact = TRUE) %||% character(0)
  adjustmentVariables = unique(as.character(adjustmentVariables))
  adjustmentVariables[nzchar(adjustmentVariables)]
}

#' Determine whether a model-output row is adjustment-related
#'
#' A row is adjustment-related when its term label includes at least one
#' selected adjustment variable.
#'
#' @param rowLabel Character scalar row label from summary/anova output.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#'
#' @return Logical scalar.
#'
#' @keywords internal
isAdjustmentRelatedOutputRow = function(rowLabel, adjustmentVariables) {
  if (termInvolvesAdjustmentVariable(termLabel = rowLabel, adjustmentVariables = adjustmentVariables)) {
    return(TRUE)
  }

  splitInteractionMembers = function(label) {
    members = character(0)
    current = ""
    inBackticks = FALSE

    chars = strsplit(label, "", fixed = TRUE)[[1]]
    for (ch in chars) {
      if (identical(ch, "`")) {
        inBackticks = !inBackticks
        current = paste0(current, ch)
      } else if (identical(ch, ":") && !inBackticks) {
        members = c(members, trimws(current))
        current = ""
      } else {
        current = paste0(current, ch)
      }
    }

    c(members, trimws(current))
  }

  stripOuterBackticks = function(x) {
    sub("^`(.*)`$", "\\1", x)
  }

  adjustmentVariables = unique(as.character(adjustmentVariables %||% character(0)))
  adjustmentVariables = adjustmentVariables[nzchar(adjustmentVariables)]
  if (length(adjustmentVariables) == 0) {
    return(FALSE)
  }

  rowMembers = stripOuterBackticks(splitInteractionMembers(rowLabel))
  adjustmentVariables = stripOuterBackticks(adjustmentVariables)

  any(vapply(
    rowMembers,
    function(member) {
      any(startsWith(member, adjustmentVariables))
    },
    logical(1)
  ))
}

#' Filter coefficient matrix rows for summary display
#'
#' @param coefficientsMatrix Matrix from `summary(model)$coefficients`.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#' @param showAdjustmentCoefficients Logical; when `TRUE` no rows are filtered.
#'
#' @return Coefficient matrix filtered for display.
#'
#' @keywords internal
filterSummaryCoefficientRows = function(coefficientsMatrix, adjustmentVariables, showAdjustmentCoefficients = FALSE) {
  if (isTRUE(showAdjustmentCoefficients) || is.null(coefficientsMatrix) || nrow(coefficientsMatrix) == 0) {
    return(coefficientsMatrix)
  }

  rowLabels = rownames(coefficientsMatrix) %||% character(0)
  if (length(rowLabels) == 0) {
    return(coefficientsMatrix)
  }

  keepRows = !vapply(
    rowLabels,
    isAdjustmentRelatedOutputRow,
    logical(1),
    adjustmentVariables = adjustmentVariables
  )

  coefficientsMatrix[keepRows, , drop = FALSE]
}

#' Filter ANOVA/deviance rows for display
#'
#' @param anovaTable ANOVA table object.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#' @param showAdjustmentTerms Logical; when `TRUE` no rows are filtered.
#'
#' @return Filtered ANOVA table.
#'
#' @keywords internal
filterAnovaTermRows = function(anovaTable, adjustmentVariables, showAdjustmentTerms = FALSE) {
  if (isTRUE(showAdjustmentTerms) || is.null(anovaTable) || nrow(anovaTable) == 0) {
    return(anovaTable)
  }

  rowLabels = rownames(anovaTable) %||% character(0)
  if (length(rowLabels) == 0) {
    return(anovaTable)
  }

  keepRows = !vapply(
    rowLabels,
    isAdjustmentRelatedOutputRow,
    logical(1),
    adjustmentVariables = adjustmentVariables
  )

  anovaTable[keepRows, , drop = FALSE]
}


#' Filter confidence-interval rows for adjustment-safe display
#'
#' @param ciTable Confidence-interval table with a `quantity` column.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#'
#' @return Confidence-interval table with adjustment-related rows removed.
#'
#' @keywords internal
filterConfidenceIntervalRows = function(ciTable, adjustmentVariables) {
  if (is.null(ciTable) || !is.data.frame(ciTable) || nrow(ciTable) == 0) {
    return(ciTable)
  }

  if (!("quantity" %in% names(ciTable))) {
    return(ciTable)
  }

  quantities = as.character(ciTable$quantity %||% "")
  keepRows = !vapply(
    quantities,
    isAdjustmentRelatedOutputRow,
    logical(1),
    adjustmentVariables = adjustmentVariables
  )

  if (length(adjustmentVariables) > 0) {
    escapedAdjustmentVariables = gsub(
      "([][{}()+*^$|\\\\?.])",
      "\\\\\\1",
      adjustmentVariables,
      perl = TRUE
    )
    adjustmentPattern = paste0("\\b(", paste(escapedAdjustmentVariables, collapse = "|"), ")\\b")
    keepRows = keepRows & !grepl(adjustmentPattern, quantities, ignore.case = TRUE, perl = TRUE)

    termLikeColumns = intersect(c("term", "contrast", "variable"), names(ciTable))
    for (columnName in termLikeColumns) {
      columnText = as.character(ciTable[[columnName]] %||% "")
      keepRows = keepRows & !grepl(adjustmentPattern, columnText, ignore.case = TRUE, perl = TRUE)
    }
  }

  ciTable[keepRows, , drop = FALSE]
}
