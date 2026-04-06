#' Score a single WMFM run record using a language model
#'
#' Sends a scoring request to the supplied chat provider and returns a pure
#' score record containing judged fields and score summaries. The input run
#' record is not modified.
#'
#' This function expects a chat provider object returned by
#' `getChatProvider()` and uses `chat$chat(prompt)`.
#'
#' @param runRecord Named list produced by `buildWmfmRunRecord()`.
#' @param chat A chat provider object as returned by `getChatProvider()`.
#' @param useCache Logical. Should scoring results be cached and reused for
#'   identical run records? Defaults to `FALSE`.
#' @param verbose Logical. Should the raw scoring response be printed?
#'   Defaults to `FALSE`.
#'
#' @return Named list containing only scored fields and LLM scoring metadata.
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

  if (is.null(chat)) {
    stop("`chat` must not be NULL.", call. = FALSE)
  }

  chatMethod = tryCatch(
    chat$chat,
    error = function(e) {
      NULL
    }
  )

  if (is.null(chatMethod) || !is.function(chatMethod)) {
    stop(
      "`chat` must provide a callable `$chat()` method.",
      call. = FALSE
    )
  }

  if (!is.logical(useCache) || length(useCache) != 1 || is.na(useCache)) {
    stop("`useCache` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  modelName = safeWmfmScalar(class(chat)[1])

  if (isTRUE(runRecord$hasError)) {
    return(list(
      llmScored = FALSE,
      llmScoringModel = modelName,
      llmScoringRaw = "",
      llmScoringSummary = "Run contains an error; LLM scoring skipped.",
      llmScoringUsedCache = FALSE,
      llmFieldReasons = NA_character_
    ))
  }

  explanationText = safeWmfmScalar(runRecord$explanationText, naString = "")
  if (!nzchar(explanationText)) {
    return(list(
      llmScored = FALSE,
      llmScoringModel = modelName,
      llmScoringRaw = "",
      llmScoringSummary = "No explanation text present; LLM scoring skipped.",
      llmScoringUsedCache = FALSE,
      llmFieldReasons = NA_character_
    ))
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
  usedCache = FALSE

  if (isTRUE(useCache) && is.environment(cacheEnv) && !is.null(cacheEnv[[key]])) {
    rawResponse = cacheEnv[[key]]
    usedCache = TRUE
  } else {
    systemPrompt = buildWmfmLlmScoringSystemPrompt()
    userPrompt = buildWmfmLlmScoringUserPrompt(runRecord)

    prompt = paste(systemPrompt, "", userPrompt, sep = "\n")

    rawResponse = chatMethod(prompt)

    if (isTRUE(useCache) && is.environment(cacheEnv)) {
      cacheEnv[[key]] = rawResponse
    }
  }

  rawResponse = safeWmfmScalar(rawResponse, naString = "")

  if (isTRUE(verbose)) {
    message(rawResponse)
  }

  parsedScores = parseWmfmScoringJson(rawResponse)

  scoreRecord = applyWmfmLlmScoresToRecord(
    parsedScores = parsedScores,
    modelName = modelName,
    rawResponse = rawResponse
  )

  scoreRecord$llmScoringUsedCache = usedCache
  scoreRecord
}
