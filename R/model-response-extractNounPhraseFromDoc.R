#' Extract a noun phrase for the response variable from dataset documentation
#'
#' Attempts to infer a short human-friendly description of the response variable
#' (e.g. "exam score", "weekly income", "probability of success") from a plain-text
#' dataset help/documentation string.
#'
#' The extraction is heuristic: it looks for common patterns such as:
#' \itemize{
#'   \item A line like \code{"responseVar: ..."} or \code{"responseVar - ..."}
#'   \item A variables table/bullets where the response variable is described
#' }
#' If nothing reliable is found, it returns \code{NULL}.
#'
#' @param dsDoc Character scalar. Plain-text dataset documentation.
#' @param responseVar Character scalar. Name of the response variable.
#' @return Character scalar noun phrase, or \code{NULL} if not found.
#' @keywords internal
extractResponseNounPhraseFromDoc = function(dsDoc, responseVar) {
  if (is.null(dsDoc) || !nzchar(dsDoc) || is.null(responseVar) || !nzchar(responseVar)) {
    return(NULL)
  }

  lines = strsplit(dsDoc, "\n", fixed = TRUE)[[1]]
  lines = trimws(lines)

  # Match patterns like:
  #   y: Exam score out of 100
  #   y - Exam score out of 100
  #   y = Exam score out of 100
  pat = paste0("^", responseVar, "\\s*[:=\\-]\\s*(.+)$")
  hit = grep(pat, lines, value = TRUE, ignore.case = FALSE)

  if (length(hit) > 0) {
    noun = sub(pat, "\\1", hit[1], perl = TRUE)
    noun = trimws(noun)

    # keep it short-ish; strip trailing full stops
    noun = sub("\\.+$", "", noun)
    if (nzchar(noun)) return(noun)
  }

  # Another common pattern in help dumps is "responseVar (unit/meaning...)"
  pat2 = paste0("^", responseVar, "\\s*\\((.+)\\)\\s*$")
  hit2 = grep(pat2, lines, value = TRUE)
  if (length(hit2) > 0) {
    noun = sub(pat2, "\\1", hit2[1], perl = TRUE)
    noun = trimws(sub("\\.+$", "", noun))
    if (nzchar(noun)) return(noun)
  }

  NULL
}
