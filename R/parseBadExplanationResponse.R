#' Parse JSON returned for bad explanation generation
#'
#' Parses the raw text returned by a language model generation call. The parser
#' is tolerant of fenced code blocks and leading or trailing non-JSON text, but
#' it expects either a single JSON object or a JSON array of objects.
#'
#' @param rawResponse Character scalar raw LLM response.
#'
#' @return A parsed list.
#' @keywords internal
#' @importFrom jsonlite fromJSON
parseBadExplanationResponse = function(rawResponse) {

  if (!is.character(rawResponse) || length(rawResponse) != 1L || is.na(rawResponse)) {
    stop("`rawResponse` must be a non-missing character scalar.", call. = FALSE)
  }

  jsonText = trimws(rawResponse)

  if (!nzchar(jsonText)) {
    stop("LLM returned an empty bad explanation response.", call. = FALSE)
  }

  jsonText = sub("^```json\\s*", "", jsonText)
  jsonText = sub("^```\\s*", "", jsonText)
  jsonText = sub("\\s*```$", "", jsonText)

  startArray = regexpr("\\[", jsonText, perl = TRUE)[1]
  endArray = tail(gregexpr("\\]", jsonText, perl = TRUE)[[1]], 1)
  startObject = regexpr("\\{", jsonText, perl = TRUE)[1]
  endObject = tail(gregexpr("\\}", jsonText, perl = TRUE)[[1]], 1)

  if (startArray != -1L && endArray != -1L && endArray > startArray) {
    jsonText = substr(jsonText, startArray, endArray)
  } else if (startObject != -1L && endObject != -1L && endObject > startObject) {
    jsonText = substr(jsonText, startObject, endObject)
  } else {
    stop("Could not locate JSON in the bad explanation response.", call. = FALSE)
  }

  parsed = tryCatch(
    jsonlite::fromJSON(jsonText, simplifyVector = FALSE),
    error = function(e) {
      stop(
        "Failed to parse bad explanation JSON: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (is.list(parsed) && !is.null(names(parsed))) {
    parsed = list(parsed)
  }

  if (!is.list(parsed)) {
    stop("Parsed bad explanation response is not a JSON list.", call. = FALSE)
  }

  parsed
}
