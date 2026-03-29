#' Score a single WMFM run record using a language model
#'
#' Sends a prompt-based scoring request to the supplied chat provider and writes
#' the returned JSON scores back into a single run record.
#'
#' This function follows the same provider pattern as the existing explanation
#' and equation helpers: it expects a chat provider object returned by
#' `getChatProvider()` and uses `chat$chat(prompt)`.
#'
#' @param runRecord Named list produced by `buildWmfmRunRecord()`.
#' @param chat A chat provider object as returned by `getChatProvider()`.
#' @param useCache Logical. Should scoring results be cached and reused for
#'   identical run records? Defaults to `FALSE` so that repeated scoring runs can
#'   vary deliberately when desired.
#' @param verbose Logical. Should the raw scoring response be printed?
#'   Defaults to `FALSE`.
#'
#' @return Updated run record with scored fields populated.
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

  if (is.null(chat) || !is.list(chat) || is.null(chat$chat) || !is.function(chat$chat)) {
    stop("`chat` must be a chat provider object with a `$chat()` method.", call. = FALSE)
  }

  if (!is.logical(useCache) || length(useCache) != 1 || is.na(useCache)) {
    stop("`useCache` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  if (isTRUE(runRecord$hasError)) {
    runRecord$llmScored = FALSE
    runRecord$llmScoringModel = safeWmfmScalar(class(chat)[1])
    runRecord$llmScoringRaw = ""
    runRecord$llmScoringSummary = "Run contains an error; LLM scoring skipped."
    return(runRecord)
  }

  explanationText = safeWmfmScalar(runRecord$explanationText, naString = "")
  if (!nzchar(explanationText)) {
    runRecord$llmScored = FALSE
    runRecord$llmScoringModel = safeWmfmScalar(class(chat)[1])
    runRecord$llmScoringRaw = ""
    runRecord$llmScoringSummary = "No explanation text present; LLM scoring skipped."
    return(runRecord)
  }

  formulaStr = safeWmfmScalar(runRecord$formula)
  explanationStr = safeWmfmScalar(runRecord$explanationText)
  equationsStr = safeWmfmScalar(runRecord$equationsText)
  interactionStr = safeWmfmScalar(runRecord$interactionTerms)
  key = paste(
    "score",
    formulaStr,
    explanationStr,
    equationsStr,
    interactionStr,
    safeWmfmScalar(runRecord$interactionMinPValue),
    sep = "|"
  )

  cacheEnv = get0(".env_cache", inherits = TRUE, ifnotfound = NULL)

  if (isTRUE(useCache) && is.environment(cacheEnv) && !is.null(cacheEnv[[key]])) {
    rawResponse = cacheEnv[[key]]
  } else {
    systemPrompt = buildWmfmLlmScoringSystemPrompt()
    userPrompt = buildWmfmLlmScoringUserPrompt(runRecord)

    prompt = paste(systemPrompt, "", userPrompt, sep = "\n")

    rawResponse = chat$chat(prompt)

    if (isTRUE(useCache) && is.environment(cacheEnv)) {
      cacheEnv[[key]] = rawResponse
    }
  }

  rawResponse = safeWmfmScalar(rawResponse, naString = "")

  if (isTRUE(verbose)) {
    message(rawResponse)
  }

  parsedScores = parseWmfmScoringJson(rawResponse)

  applyWmfmLlmScoresToRecord(
    runRecord = runRecord,
    parsedScores = parsedScores,
    modelName = safeWmfmScalar(class(chat)[1]),
    rawResponse = rawResponse
  )
}
