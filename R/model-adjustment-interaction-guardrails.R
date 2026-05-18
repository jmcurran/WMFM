#' Detect whether a model term involves adjustment variables
#'
#' Checks whether a term label (main effect or interaction) includes at least
#' one variable currently marked as an adjustment variable.
#'
#' @param termLabel Character scalar term label, such as \code{"x"} or
#'   \code{"x:z"}.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#'
#' @return Logical scalar indicating whether the term involves an adjustment
#'   variable.
#' @keywords internal
termInvolvesAdjustmentVariable = function(termLabel, adjustmentVariables) {
  if (!is.character(termLabel) || length(termLabel) != 1 || !nzchar(termLabel)) {
    return(FALSE)
  }

  adjustmentVariables = unique(as.character(adjustmentVariables %||% character(0)))
  adjustmentVariables = adjustmentVariables[nzchar(adjustmentVariables)]

  if (length(adjustmentVariables) == 0) {
    return(FALSE)
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

  termVariables = splitInteractionMembers(termLabel)
  any(termVariables %in% adjustmentVariables)
}

