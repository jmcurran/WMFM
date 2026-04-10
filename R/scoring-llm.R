#' Build a pure LLM score record from parsed scores
#'
#' Validates a parsed LLM scoring response and returns a score record
#' containing only judged fields, score summaries, and LLM scoring metadata.
#'
#' @param parsedScores Named list produced by `parseWmfmScoringJson()`.
#' @param modelName Optional character identifier for the scoring model.
#' @param rawResponse Raw character response returned by the LLM.
#'
#' @return Named list containing only scoring fields.
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
applyWmfmLlmScoresToRecord = function(
    parsedScores,
    modelName = NA_character_,
    rawResponse = NA_character_
) {
  parsedScores = validateWmfmParsedScores(parsedScores)

  scoreRecord = list(
    effectDirectionCorrect = parsedScores$effectDirectionCorrect,
    effectScaleAppropriate = parsedScores$effectScaleAppropriate,
    referenceGroupHandledCorrectly = parsedScores$referenceGroupHandledCorrectly,
    interactionCoverageAdequate = parsedScores$interactionCoverageAdequate,
    interactionSubstantiveCorrect = parsedScores$interactionSubstantiveCorrect,
    uncertaintyHandlingAppropriate = parsedScores$uncertaintyHandlingAppropriate,
    inferentialRegisterAppropriate = parsedScores$inferentialRegisterAppropriate,
    mainEffectCoverageAdequate = parsedScores$mainEffectCoverageAdequate,
    referenceGroupCoverageAdequate = parsedScores$referenceGroupCoverageAdequate,
    clarityAdequate = parsedScores$clarityAdequate,
    numericExpressionAdequate = parsedScores$numericExpressionAdequate,
    comparisonStructureClear = parsedScores$comparisonStructureClear,
    fatalFlawDetected = parsedScores$fatalFlawDetected,
    factualScore = parsedScores$factualScore,
    inferenceScore = parsedScores$inferenceScore,
    completenessScore = parsedScores$completenessScore,
    clarityScore = parsedScores$clarityScore,
    calibrationScore = parsedScores$calibrationScore,
    overallScore = parsedScores$overallScore,
    overallPass = parsedScores$overallPass,
    llmScored = TRUE,
    llmScoringModel = safeWmfmScalar(modelName),
    llmScoringRaw = safeWmfmScalar(rawResponse, naString = ""),
    llmScoringSummary = parsedScores$llmScoringSummary,
    llmFieldReasons = jsonlite::toJSON(
      parsedScores$fieldReasons,
      auto_unbox = TRUE,
      null = "null"
    )
  )

  scoreRecord
}


#' Compute the number of WMFM LLM grading jobs
#'
#' Internal helper for estimating how many LLM grading calls a request would
#' make.
#'
#' @param nExplanations Integer. Number of explanations.
#' @param method Character. One of `"deterministic"`, `"llm"`, or `"both"`.
#' @param nLlm Integer. Number of repeated LLM gradings per explanation.
#'
#' @return Integer scalar.
#' @keywords internal
computeWmfmLlmJobCount = function(
    nExplanations,
    method,
    nLlm = 1L
) {

  nExplanations = as.integer(nExplanations)[1]
  nLlm = as.integer(nLlm)[1]
  method = match.arg(method, c("deterministic", "llm", "both"))

  if (is.na(nExplanations) || nExplanations < 0L) {
    stop("`nExplanations` must be a non-negative integer.", call. = FALSE)
  }

  if (is.na(nLlm) || nLlm < 1L) {
    stop("`nLlm` must be an integer greater than or equal to 1.", call. = FALSE)
  }

  if (identical(method, "deterministic")) {
    return(0L)
  }

  as.integer(nExplanations * nLlm)
}


#' Enforce the WMFM LLM grading job guard
#'
#' Internal helper that prevents unexpectedly large LLM grading requests unless
#' the user explicitly opts in.
#'
#' @param totalLlmCalls Integer. Total planned LLM calls.
#' @param nExplanations Integer. Number of explanations.
#' @param nLlm Integer. Number of repeated LLM gradings per explanation.
#' @param confirmLargeLlmJob Logical. Whether to allow large requests.
#' @param maxLlmJobsWithoutConfirmation Integer. Maximum number of LLM calls
#'   allowed without explicit confirmation.
#'
#' @return Invisibly returns `TRUE` when the request is allowed.
#' @keywords internal
enforceWmfmLlmJobGuard = function(
    totalLlmCalls,
    nExplanations,
    nLlm,
    confirmLargeLlmJob = FALSE,
    maxLlmJobsWithoutConfirmation = 20L
) {

  totalLlmCalls = as.integer(totalLlmCalls)[1]
  nExplanations = as.integer(nExplanations)[1]
  nLlm = as.integer(nLlm)[1]
  maxLlmJobsWithoutConfirmation = as.integer(maxLlmJobsWithoutConfirmation)[1]

  if (is.na(totalLlmCalls) || totalLlmCalls < 0L) {
    stop("`totalLlmCalls` must be a non-negative integer.", call. = FALSE)
  }

  if (!is.logical(confirmLargeLlmJob) || length(confirmLargeLlmJob) != 1 || is.na(confirmLargeLlmJob)) {
    stop("`confirmLargeLlmJob` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.na(maxLlmJobsWithoutConfirmation) || maxLlmJobsWithoutConfirmation < 0L) {
    stop(
      "`maxLlmJobsWithoutConfirmation` must be a non-negative integer.",
      call. = FALSE
    )
  }

  if (totalLlmCalls > maxLlmJobsWithoutConfirmation && !isTRUE(confirmLargeLlmJob)) {
    stop(
      "This grading request would make ", totalLlmCalls, " LLM calls (",
      nExplanations, " explanation", if (nExplanations == 1L) "" else "s",
      " x ", nLlm, " repeated LLM grading",
      if (nLlm == 1L) "" else "s",
      "). Set `confirmLargeLlmJob = TRUE` to proceed.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


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
#' @importFrom utils tail
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


#' Score multiple WMFM runs using an LLM
#'
#' Scores each raw run record in a `wmfmRuns` object using an LLM-based scorer.
#' The returned value is a list of score records containing only judged fields
#' and score summaries. The input run records are not modified.
#'
#' Timing metadata is attached to the returned list as an attribute named
#' `"timing"`.
#'
#' @param runRecords List of raw WMFM run records.
#' @param chat Chat provider object used for LLM scoring.
#' @param useCache Logical. Whether to allow cached LLM responses.
#' @param showProgress Logical. Whether to display progress and timing
#'   information during scoring.
#' @param verbose Logical. Whether to print additional diagnostic information.
#'
#' @return A list of score records. Each element corresponds to one input run
#'   record and contains only judged fields and score summaries. A `"timing"`
#'   attribute is attached to the returned list.
#' @export
scoreWmfmRunsWithLlm = function(
    runRecords,
    chat,
    useCache = FALSE,
    showProgress = TRUE,
    verbose = FALSE
) {
  if (!is.list(runRecords) || length(runRecords) == 0) {
    stop("`runRecords` must be a non-empty list of raw run records.", call. = FALSE)
  }

  if (missing(chat) || is.null(chat)) {
    stop("`chat` must be supplied for LLM scoring.", call. = FALSE)
  }

  nRuns = length(runRecords)
  scoredRuns = vector("list", nRuns)
  iterationSeconds = numeric(nRuns)

  startedAt = Sys.time()

  progressBar = NULL
  if (isTRUE(showProgress)) {
    progressBar = utils::txtProgressBar(
      min = 0,
      max = nRuns,
      initial = 0,
      style = 3
    )

    on.exit(
      close(progressBar),
      add = TRUE
    )
  }

  for (i in seq_len(nRuns)) {
    iterationStart = Sys.time()

    scoredRuns[[i]] = scoreWmfmRunWithLlm(
      runRecord = runRecords[[i]],
      chat = chat,
      useCache = useCache,
      verbose = verbose
    )

    if (!is.list(scoredRuns[[i]]) || length(scoredRuns[[i]]) == 0) {
      stop(
        sprintf(
          "LLM scoring failed to return a valid score record for run %d.",
          i
        ),
        call. = FALSE
      )
    }

    iterationSeconds[i] = as.numeric(
      difftime(Sys.time(), iterationStart, units = "secs")
    )

    if (isTRUE(showProgress)) {
      utils::setTxtProgressBar(progressBar, i)

      avgSeconds = mean(iterationSeconds[seq_len(i)])
      remainingRuns = nRuns - i
      etaSeconds = avgSeconds * remainingRuns

      message(
        sprintf(
          "\nRun %d/%d completed in %.2f s | elapsed %.2f s | ETA %.2f s",
          i,
          nRuns,
          iterationSeconds[i],
          sum(iterationSeconds[seq_len(i)]),
          etaSeconds
        )
      )
    }
  }

  finishedAt = Sys.time()

  timing = list(
    startedAt = as.character(startedAt),
    finishedAt = as.character(finishedAt),
    iterationSeconds = iterationSeconds,
    averageIterationSeconds = mean(iterationSeconds),
    totalElapsedSeconds = as.numeric(
      difftime(finishedAt, startedAt, units = "secs")
    )
  )

  attr(scoredRuns, "timing") = timing

  scoredRuns
}


#' Build the system prompt for WMFM explanation scoring
#'
#' Creates the fixed system prompt used when asking a language model to score a
#' plain-language model explanation against the WMFM scoring schema.
#'
#' @return A character scalar containing the system prompt.
#' @keywords internal
buildWmfmLlmScoringSystemPrompt = function() {

  paste(
    "You are scoring a plain-language explanation of a fitted statistical model.",
    "You must score the explanation against the supplied fitted-model context.",
    "Be strict, consistent, and conservative.",
    "",
    "Scoring rules:",
    "- Distinguish clearly between descriptive wording and inferential wording.",
    "- Penalise incorrect effect direction, incorrect effect scale, incorrect baseline/reference handling, and incorrect interaction interpretation.",
    "- Penalise causal language unless the supplied model context explicitly justifies causation.",
    "- If interaction terms are present, assess whether the explanation covers them adequately and correctly.",
    "- If interaction terms are absent, penalise invented interaction claims.",
    "- Penalise overclaiming more heavily than cautious wording.",
    "- Use only the information provided in the prompt.",
    "",
    "Output rules:",
    "- Return strict JSON only.",
    "- Do not wrap the JSON in markdown unless the provider forces it.",
    "- Use the exact field names requested.",
    "- For rubric fields scored on the 0/1/2 scale:",
    "  2 = correct, appropriate, adequate, or clear",
    "  1 = partly correct, incomplete, mixed, or somewhat unclear",
    "  0 = clearly wrong, inappropriate, missing when important, or seriously unclear",
    "",
    "Aggregate score rules:",
    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be numeric values between 0 and 2 inclusive.",
    "- overallScore must be a numeric value between 0 and 100 inclusive.",
    "- overallPass should usually be FALSE if fatalFlawDetected is TRUE.",
    sep = "\n"
  )
}


#' Build a user prompt for WMFM explanation scoring
#'
#' Given a single run record produced by `buildWmfmRunRecord()`, construct a
#' prompt asking a language model to score the explanation and return strict
#' JSON matching the WMFM scoring schema.
#'
#' This prompt includes field-specific rubric guidance so that the language
#' model applies the WMFM scoring dimensions consistently rather than inferring
#' its own definitions of what counts as adequate, clear, or appropriate.
#'
#' @param runRecord Named list produced by `buildWmfmRunRecord()`.
#'
#' @return A character scalar containing the prompt text.
#' @keywords internal
buildWmfmLlmScoringUserPrompt = function(runRecord) {

  if (!is.list(runRecord)) {
    stop("`runRecord` must be a named list.", call. = FALSE)
  }

  requiredFields = c(
    "exampleName",
    "package",
    "modelType",
    "formula",
    "equationsText",
    "explanationText",
    "interactionTerms",
    "hasInteractionTerms",
    "nInteractionTerms",
    "interactionMinPValue",
    "interactionAlpha"
  )

  missingFields = setdiff(requiredFields, names(runRecord))
  if (length(missingFields) > 0) {
    stop(
      "runRecord is missing required fields: ",
      paste(missingFields, collapse = ", "),
      call. = FALSE
    )
  }

  interactionBlock =
    if (isTRUE(runRecord$hasInteractionTerms)) {
      paste(
        "Interaction terms are present in the fitted model.",
        paste0("Interaction terms: ", safeWmfmScalar(runRecord$interactionTerms)),
        paste0("Number of interaction terms: ", safeWmfmScalar(runRecord$nInteractionTerms)),
        paste0("Minimum interaction p-value: ", safeWmfmScalar(runRecord$interactionMinPValue)),
        paste0("Interaction alpha threshold: ", safeWmfmScalar(runRecord$interactionAlpha)),
        sep = "\n"
      )
    } else {
      "No interaction terms are present in the fitted model."
    }

  fieldSpecificRubric = paste(
    "FIELD-SPECIFIC RUBRIC GUIDANCE",
    "- effectDirectionCorrect:",
    "  2 if the explanation gives the correct direction of the main effect(s); 1 if only partly correct, mixed, or incomplete; 0 if direction is wrong or missing when important.",
    "- effectScaleAppropriate:",
    "  2 if the explanation uses the correct effect scale for the model context (for example additive differences in points, multiplicative changes, probabilities, or odds); 1 if the scale is only partly clear or mixed; 0 if the scale is wrong, seriously confused, or absent when important.",
    "- referenceGroupHandledCorrectly:",
    "  2 if the baseline or comparison group is handled correctly; 1 if only partly clear; 0 if the baseline or comparison is wrong or materially misleading.",
    "- interactionCoverageAdequate:",
    "  2 if important interactions are clearly covered when present; 1 if interaction coverage is partial; 0 if an important interaction is missed or an interaction is invented.",
    "- interactionSubstantiveCorrect:",
    "  2 if the explanation gets the substantive interpretation of the interaction right; 1 if partly right or vague; 0 if wrong.",
    "- uncertaintyHandlingAppropriate:",
    "  2 if uncertainty or evidential qualification is handled appropriately; 1 if present but limited; 0 if badly mishandled or absent in a way that encourages overstatement.",
    "- inferentialRegisterAppropriate:",
    "  2 if the wording matches the evidential strength and avoids inappropriate causal claims; 1 if somewhat mixed; 0 if clearly overclaiming or otherwise inappropriate.",
    "- mainEffectCoverageAdequate:",
    "  2 if the key main effects are adequately covered; 1 if partly covered; 0 if important main effects are omitted.",
    "- referenceGroupCoverageAdequate:",
    "  2 if the explanation adequately communicates the baseline or comparison structure when relevant; 1 if partial; 0 if missing when important.",
    "- clarityAdequate:",
    "  2 if the explanation is generally clear and easy to follow; 1 if understandable but somewhat awkward, wordy, or uneven; 0 if seriously unclear.",
    "- numericExpressionAdequate:",
    "  2 if the explanation gives a clear quantitative interpretation of one or more important model effects using meaningful numbers, units, point differences, probabilities, odds, multiplicative language, or intervals where appropriate.",
    "  1 if some numeric information is present but the quantitative interpretation is partial, weakly connected to the effect, vague, or somewhat unclear.",
    "  0 if meaningful quantitative interpretation is absent when it should be present, or if the numeric expression is seriously misleading.",
    "  Minor stylistic awkwardness alone should not reduce a score from 2 to 1 if the quantitative interpretation is still clear.",
    "  Do not treat percentage language about model fit, such as R-squared or percent of variation explained, as evidence that the coefficient effect scale is multiplicative.",
    "- comparisonStructureClear:",
    "  2 if relevant comparisons or conditional structures are clearly expressed; 1 if partly clear; 0 if important comparison structure is missing or confusing.",
    "- fatalFlawDetected:",
    "  TRUE only for a serious problem such as a major directional error, clear overclaiming, invented interaction, or another flaw that should strongly affect the final judgment.",
    sep = "\n"
  )

  jsonTemplate = paste(
    "{",
    '  "effectDirectionCorrect": 0,',
    '  "effectScaleAppropriate": 0,',
    '  "referenceGroupHandledCorrectly": 0,',
    '  "interactionCoverageAdequate": 0,',
    '  "interactionSubstantiveCorrect": 0,',
    '  "uncertaintyHandlingAppropriate": 0,',
    '  "inferentialRegisterAppropriate": 0,',
    '  "mainEffectCoverageAdequate": 0,',
    '  "referenceGroupCoverageAdequate": 0,',
    '  "clarityAdequate": 0,',
    '  "numericExpressionAdequate": 0,',
    '  "comparisonStructureClear": 0,',
    '  "fatalFlawDetected": false,',
    '  "factualScore": 0.0,',
    '  "inferenceScore": 0.0,',
    '  "completenessScore": 0.0,',
    '  "clarityScore": 0.0,',
    '  "calibrationScore": 0.0,',
    '  "overallScore": 0.0,',
    '  "overallPass": false,',
    '  "llmScoringSummary": "Short summary.",',
    '  "fieldReasons": {',
    '    "effectDirectionCorrect": "brief reason",',
    '    "effectScaleAppropriate": "brief reason",',
    '    "referenceGroupHandledCorrectly": "brief reason",',
    '    "interactionCoverageAdequate": "brief reason",',
    '    "interactionSubstantiveCorrect": "brief reason",',
    '    "uncertaintyHandlingAppropriate": "brief reason",',
    '    "inferentialRegisterAppropriate": "brief reason",',
    '    "mainEffectCoverageAdequate": "brief reason",',
    '    "referenceGroupCoverageAdequate": "brief reason",',
    '    "clarityAdequate": "brief reason",',
    '    "numericExpressionAdequate": "brief reason",',
    '    "comparisonStructureClear": "brief reason",',
    '    "fatalFlawDetected": "brief reason"',
    '  }',
    "}",
    sep = "\n"
  )

  paste(
    "Please score the following WMFM explanation.",
    "",
    "MODEL CONTEXT",
    paste0("Example name: ", safeWmfmScalar(runRecord$exampleName)),
    paste0("Package: ", safeWmfmScalar(runRecord$package)),
    paste0("Model type: ", safeWmfmScalar(runRecord$modelType)),
    paste0("Formula: ", safeWmfmScalar(runRecord$formula)),
    "",
    "FITTED EQUATIONS",
    safeWmfmScalar(runRecord$equationsText),
    "",
    "INTERACTION CONTEXT",
    interactionBlock,
    "",
    "EXPLANATION TO SCORE",
    safeWmfmScalar(runRecord$explanationText),
    "",
    fieldSpecificRubric,
    "",
    "Return strict JSON with exactly these top-level fields:",
    "effectDirectionCorrect",
    "effectScaleAppropriate",
    "referenceGroupHandledCorrectly",
    "interactionCoverageAdequate",
    "interactionSubstantiveCorrect",
    "uncertaintyHandlingAppropriate",
    "inferentialRegisterAppropriate",
    "mainEffectCoverageAdequate",
    "referenceGroupCoverageAdequate",
    "clarityAdequate",
    "numericExpressionAdequate",
    "comparisonStructureClear",
    "fatalFlawDetected",
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore",
    "overallScore",
    "overallPass",
    "llmScoringSummary",
    "fieldReasons",
    "",
    "Important score-scale reminder:",
    "- factualScore, inferenceScore, completenessScore, clarityScore, and calibrationScore must each be between 0 and 2.",
    "- overallScore must be between 0 and 100.",
    "",
    "Use this JSON shape:",
    jsonTemplate,
    "",
    "Important reminders:",
    "- Do not add any extra top-level fields.",
    "- Do not omit required fields.",
    "- fieldReasons must itself be a JSON object with the required reason fields.",
    "- Keep reasons brief.",
    "- Return JSON only.",
    sep = "\n"
  )
}


#' Validate parsed WMFM LLM scores
#'
#' Validates the structure and allowable values of a parsed JSON scoring
#' response before it is written back into a run record.
#'
#' @param parsedScores Named list produced by `parseWmfmScoringJson()`.
#'
#' @return The validated and type-normalised score list.
#' @keywords internal
validateWmfmParsedScores = function(parsedScores) {

  if (!is.list(parsedScores) || is.null(names(parsedScores))) {
    stop("`parsedScores` must be a named list.", call. = FALSE)
  }

  rubricFields = c(
    "effectDirectionCorrect",
    "effectScaleAppropriate",
    "referenceGroupHandledCorrectly",
    "interactionCoverageAdequate",
    "interactionSubstantiveCorrect",
    "uncertaintyHandlingAppropriate",
    "inferentialRegisterAppropriate",
    "mainEffectCoverageAdequate",
    "referenceGroupCoverageAdequate",
    "clarityAdequate",
    "numericExpressionAdequate",
    "comparisonStructureClear"
  )

  dimensionScoreFields = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore"
  )

  logicalFields = c(
    "fatalFlawDetected",
    "overallPass"
  )

  characterFields = c(
    "llmScoringSummary"
  )

  requiredReasonFields = c(
    rubricFields,
    "fatalFlawDetected"
  )

  requiredFields = c(
    rubricFields,
    dimensionScoreFields,
    "overallScore",
    logicalFields,
    characterFields,
    "fieldReasons"
  )

  missingFields = setdiff(requiredFields, names(parsedScores))
  if (length(missingFields) > 0) {
    stop(
      "Parsed scoring response is missing required fields: ",
      paste(missingFields, collapse = ", "),
      call. = FALSE
    )
  }

  out = parsedScores

  for (field in rubricFields) {
    value = suppressWarnings(as.integer(out[[field]]))
    if (length(value) != 1 || is.na(value) || !(value %in% c(0L, 1L, 2L))) {
      stop(
        "Field `", field, "` must be one of 0, 1, or 2.",
        call. = FALSE
      )
    }
    out[[field]] = value
  }

  for (field in dimensionScoreFields) {
    value = suppressWarnings(as.numeric(out[[field]]))
    if (length(value) != 1 || is.na(value) || !is.finite(value)) {
      stop("Field `", field, "` must be a finite numeric scalar.", call. = FALSE)
    }
    if (value < 0 || value > 2) {
      stop("Field `", field, "` must lie between 0 and 2.", call. = FALSE)
    }
    out[[field]] = value
  }

  overallScore = suppressWarnings(as.numeric(out$overallScore))
  if (length(overallScore) != 1 || is.na(overallScore) || !is.finite(overallScore)) {
    stop("Field `overallScore` must be a finite numeric scalar.", call. = FALSE)
  }
  if (overallScore < 0 || overallScore > 100) {
    stop("Field `overallScore` must lie between 0 and 100.", call. = FALSE)
  }
  out$overallScore = overallScore

  for (field in logicalFields) {
    value = out[[field]]
    if (is.character(value)) {
      lowerValue = tolower(trimws(value[1]))
      if (lowerValue %in% c("true", "t")) {
        value = TRUE
      } else if (lowerValue %in% c("false", "f")) {
        value = FALSE
      }
    }
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
      stop("Field `", field, "` must be TRUE or FALSE.", call. = FALSE)
    }
    out[[field]] = value
  }

  for (field in characterFields) {
    value = safeWmfmScalar(out[[field]], naString = "")
    if (!nzchar(value)) {
      stop("Field `", field, "` must be a non-empty character scalar.", call. = FALSE)
    }
    out[[field]] = value
  }

  reasons = out$fieldReasons
  if (!is.list(reasons) || is.null(names(reasons))) {
    stop("Field `fieldReasons` must be a named JSON object.", call. = FALSE)
  }

  if (!"fatalFlawDetected" %in% names(reasons)) {
    reasons$fatalFlawDetected =
      if (isTRUE(out$fatalFlawDetected)) {
        "A fatal flaw was detected based on the explanation."
      } else {
        "No fatal flaw was detected."
      }
  }

  missingReasonFields = setdiff(requiredReasonFields, names(reasons))
  if (length(missingReasonFields) > 0) {
    stop(
      "fieldReasons is missing required fields: ",
      paste(missingReasonFields, collapse = ", "),
      call. = FALSE
    )
  }

  reasons = lapply(reasons, function(x) {
    safeWmfmScalar(x, naString = "", singleLine = TRUE)
  })

  emptyReasonFields = names(reasons)[names(reasons) %in% requiredReasonFields & !nzchar(unlist(reasons))]
  if (length(emptyReasonFields) > 0) {
    stop(
      "fieldReasons contains empty required fields: ",
      paste(emptyReasonFields, collapse = ", "),
      call. = FALSE
    )
  }

  out$fieldReasons = reasons
  out
}
