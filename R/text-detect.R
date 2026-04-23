#' Detect pattern in text
#'
#' @param x Character text.
#' @param pattern Regex pattern.
#'
#' @return Logical.
#' @export
detectWmfmPattern = function(x, pattern) {
  if (is.na(x)) return(NA)
  grepl(pattern, x, ignore.case = TRUE)
}
#' Detect implicit comparison language in WMFM explanations
#'
#' Detects comparison structure that may not use explicit phrases such as
#' "compared with" or "relative to", but still clearly expresses a contrast
#' between groups or conditions.
#'
#' This helper is intended to support extraction in `buildWmfmRunRecord()`,
#' especially for explanations that use constructions such as:
#' \itemize{
#'   \item "higher ... than those who did not"
#'   \item "more ... than students who did not attend"
#'   \item "lower ... than those without ..."
#' }
#'
#' The function looks for comparative wording together with at least one
#' structural cue indicating contrast or grouping.
#'
#' @param text Character scalar explanation text.
#'
#' @return Logical scalar. `TRUE` if implicit comparison language is detected,
#'   otherwise `FALSE`.
#' @export
detectImplicitComparison = function(text) {

  if (length(text) == 0 || is.na(text) || !nzchar(trimws(text))) {
    return(FALSE)
  }

  text = as.character(text)[1]

  hasGroup = grepl(
    paste(
      "\\bstudents who\\b",
      "\\bthose who\\b",
      "\\bpeople who\\b",
      "\\bindividuals who\\b",
      "\\bparticipants who\\b",
      sep = "|"
    ),
    text,
    ignore.case = TRUE,
    perl = TRUE
  )

  hasContrast = grepl(
    paste(
      "\\bdid not\\b",
      "\\bdo not\\b",
      "\\bdoes not\\b",
      "\\bwithout\\b",
      "\\bnon-\\w+\\b",
      "\\bno\\b",
      sep = "|"
    ),
    text,
    ignore.case = TRUE,
    perl = TRUE
  )

  hasComparative = grepl(
    paste(
      "\\bhigher\\b",
      "\\blower\\b",
      "\\bmore\\b",
      "\\bless\\b",
      "\\bincrease\\b",
      "\\bincreased\\b",
      "\\bdecrease\\b",
      "\\bdecreased\\b",
      "\\bgreater\\b",
      "\\bsmaller\\b",
      sep = "|"
    ),
    text,
    ignore.case = TRUE,
    perl = TRUE
  )

  hasThan = grepl(
    "\\bthan\\b",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )

  (hasComparative && (hasContrast || hasGroup || hasThan))
}
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
