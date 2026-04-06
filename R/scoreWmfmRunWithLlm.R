#' Score a single WMFM run using an LLM
#'
#' Takes a raw run record and returns a scored record containing only
#' judged fields and score summaries. The input run record is not modified.
#'
#' @param runRecord A single raw run record (named list).
#' @param chat Chat provider.
#' @param useCache Logical. Whether to use caching.
#' @param verbose Logical. Whether to print debug information.
#'
#' @return Named list containing only scoring fields.
#' @export
scoreWmfmRunWithLlm = function(
    runRecord,
    chat,
    useCache = FALSE,
    verbose = FALSE
) {
  if (!is.list(runRecord) || is.null(names(runRecord))) {
    stop("`runRecord` must be a named list.", call. = FALSE)
  }

  prompt = buildWmfmScoringPrompt(runRecord)

  response = chat(
    prompt,
    useCache = useCache,
    verbose = verbose
  )

  parsed = parseWmfmLlmScoringResponse(response)

  if (!is.list(parsed) || length(parsed) == 0) {
    stop("LLM scoring returned an empty or invalid result.", call. = FALSE)
  }

  parsed
}
