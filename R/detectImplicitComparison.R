#' Detect implicit comparison language in WMFM explanation text
#'
#' Detects implicit comparison structures that may indicate group contrasts
#' even when the explanation does not use a short explicit phrase such as
#' "higher than" or "compared with". This is intended as a fallback helper
#' for claim extraction in `buildWmfmRunRecord()`.
#'
#' The current heuristic looks for all of the following:
#' \itemize{
#'   \item a group-referencing phrase such as "students who" or "those who",
#'   \item a contrast marker such as "did not" or "without", and
#'   \item a comparative signal such as "higher", "lower", "more", or
#'   "less".
#' }
#'
#' This increases recall for explanations that clearly compare groups in
#' natural language without explicitly naming a reference group.
#'
#' @param text Character scalar explanation text.
#'
#' @return Logical scalar. `TRUE` if implicit comparison language is detected,
#'   otherwise `FALSE`.
#' @export
#'
#' @examples
#' detectImplicitComparison(
#'   "Students who attended class had higher exam scores than those who did not."
#' )
detectImplicitComparison = function(text) {

  if (length(text) == 0 || is.na(text) || !nzchar(trimws(text))) {
    return(FALSE)
  }

  groupPattern = paste(
    "\bthose who\b",
    "\bstudents who\b",
    "\bpeople who\b",
    "\bindividuals who\b",
    sep = "|"
  )

  contrastPattern = paste(
    "\bdid not\b",
    "\bdidn't\b",
    "\bwithout\b",
    "\bno\b",
    sep = "|"
  )

  comparativePattern = paste(
    "\bhigher\b",
    "\blower\b",
    "\bmore\b",
    "\bless\b",
    "\bincrease(s|d|ing)?\b",
    "\bdecrease(s|d|ing)?\b",
    sep = "|"
  )

  hasGroupReference = grepl(groupPattern, text, ignore.case = TRUE, perl = TRUE)
  hasContrastMarker = grepl(contrastPattern, text, ignore.case = TRUE, perl = TRUE)
  hasComparativeSignal = grepl(comparativePattern, text, ignore.case = TRUE, perl = TRUE)

  (hasComparative && hasContrast) ||
    (hasComparative && hasGroup)
}
