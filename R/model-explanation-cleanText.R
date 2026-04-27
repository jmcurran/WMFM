#' Clean generated explanation text before deterministic processing
#'
#' Removes simple LLM formatting artifacts that can leak into the visible
#' explanation, such as leading `Answer`, `Answer:`, or `Answer -` tokens.
#' This is a deterministic surface cleanup step. It does not rewrite the
#' statistical content of the explanation.
#'
#' @param text Character vector of explanation text.
#'
#' @return A character vector with formatting artifacts removed.
#' @export
cleanExplanationText = function(text) {

  if (is.null(text)) {
    return(NULL)
  }

  if (!is.character(text)) {
    stop("`text` must be a character vector or NULL.", call. = FALSE)
  }

  cleaned = text
  keep = !is.na(cleaned)

  cleaned[keep] = gsub(
    pattern = paste0(
      "(^|(?<=[.!?]\\s)|(?<=[.!?]\\n)|(?<=\\n))",
      "[[:space:]]*",
      "(?:[#>*_`~-]+[[:space:]]*)*",
      "answer",
      "(?:[[:space:]]+to[[:space:]]+the[[:space:]]+research[[:space:]]+question)?",
      "\\b",
      "[[:space:]]*(?::|-|--|\\.)?",
      "[[:space:]]*",
      "(?:[#>*_`~-]+[[:space:]]*)*"
    ),
    replacement = "\\1",
    x = cleaned[keep],
    perl = TRUE,
    ignore.case = TRUE
  )

  cleaned[keep] = trimws(cleaned[keep])
  cleaned
}
