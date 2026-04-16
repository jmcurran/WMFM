#' Format a deterministic equation table for display
#'
#' Converts a `wmfmEquationTable` into a stable plain-text representation for
#' console output, text extraction, and downstream grading records.
#'
#' @param x A `wmfmEquationTable` object.
#' @param includeCondition Logical. Should condition labels be included?
#'   Defaults to `TRUE`.
#'
#' @return Character vector of formatted display lines.
#' @keywords internal
formatWmfmEquationTableLines = function(
    x,
    includeCondition = TRUE
) {

  if (!inherits(x, "wmfmEquationTable")) {
    stop("`x` must inherit from `wmfmEquationTable`.", call. = FALSE)
  }

  if (!is.logical(includeCondition) || length(includeCondition) != 1 || is.na(includeCondition)) {
    stop("`includeCondition` must be TRUE or FALSE.", call. = FALSE)
  }

  buildRowLines = function(i) {
    pieces = character(0)

    if (isTRUE(includeCondition) && "condition" %in% names(x)) {
      conditionValue = x$condition[[i]]

      if (!is.na(conditionValue) && nzchar(trimws(conditionValue))) {
        pieces = c(pieces, paste0("Condition: ", conditionValue))
      }
    }

    for (fieldName in c("linearPredictor", "oddsScale", "responseScale")) {
      if (!(fieldName %in% names(x))) {
        next
      }

      value = x[[fieldName]][[i]]

      if (!is.na(value) && nzchar(trimws(value))) {
        pieces = c(pieces, value)
      }
    }

    if (length(pieces) == 0 && "equation" %in% names(x)) {
      value = x$equation[[i]]

      if (!is.na(value) && nzchar(trimws(value))) {
        pieces = c(pieces, value)
      }
    }

    pieces
  }

  blocks = lapply(seq_len(nrow(x)), buildRowLines)
  blocks = Filter(length, blocks)

  if (length(blocks) == 0) {
    return(character(0))
  }

  unlist(
    lapply(seq_along(blocks), function(i) {
      block = blocks[[i]]

      if (i < length(blocks)) {
        c(block, "")
      } else {
        block
      }
    }),
    use.names = FALSE
  )
}
