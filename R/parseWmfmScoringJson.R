#' Parse JSON returned by the LLM scorer
#'
#' Parses the raw text returned by a language model scoring call. The parser is
#' tolerant of fenced code blocks and leading or trailing non-JSON text, but it
#' expects exactly one JSON object to be present.
#'
#' @param rawResponse Character scalar containing the model response.
#'
#' @return A named list parsed from JSON.
#' @keywords internal
#'
#' @importFrom jsonlite fromJSON
parseWmfmScoringJson = function(rawResponse) {

  if (!is.character(rawResponse) || length(rawResponse) != 1 || is.na(rawResponse)) {
    stop("`rawResponse` must be a non-missing character scalar.", call. = FALSE)
  }

  jsonText = trimws(rawResponse)

  if (!nzchar(jsonText)) {
    stop("LLM returned an empty scoring response.", call. = FALSE)
  }

  jsonText = sub("^```json\\s*", "", jsonText)
  jsonText = sub("^```\\s*", "", jsonText)
  jsonText = sub("\\s*```$", "", jsonText)

  startPos = regexpr("\\{", jsonText, perl = TRUE)[1]
  endPos = tail(gregexpr("\\}", jsonText, perl = TRUE)[[1]], 1)

  if (startPos == -1 || endPos == -1 || endPos < startPos) {
    stop("Could not locate a JSON object in the LLM response.", call. = FALSE)
  }

  jsonText = substr(jsonText, startPos, endPos)

  parsed = tryCatch(
    jsonlite::fromJSON(jsonText, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse LLM scoring JSON: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!is.list(parsed) || is.null(names(parsed))) {
    stop("Parsed scoring response is not a named JSON object.", call. = FALSE)
  }

  parsed
}
