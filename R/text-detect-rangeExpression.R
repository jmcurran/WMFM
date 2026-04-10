#' Detect numeric range expressions (implicit uncertainty)
#'
#' Detects expressions like "from 3.0 to 4.0" or
#' "between 3.8 and 12.2", which indicate uncertainty
#' without explicitly mentioning confidence intervals.
#'
#' @param text Character string
#'
#' @return Logical
#' @export
detectRangeExpression = function(text) {

  if (length(text) == 0 || is.na(text) || !nzchar(trimws(text))) {
    return(FALSE)
  }

  text = as.character(text)[1]

  hasBetween = grepl(
    "between\\s+[0-9\\.]+\\s+and\\s+[0-9\\.]+",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )

  hasFromTo = grepl(
    "from\\s+[0-9\\.]+\\s+to\\s+[0-9\\.]+",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )

  hasBetween || hasFromTo
}
