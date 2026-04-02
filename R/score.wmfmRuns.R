#' Score a WMFM runs object
#'
#' Scores a `wmfmRuns` object using deterministic scoring, LLM scoring, or both,
#' and returns a separate `wmfmScores` object.
#'
#' @param x A `wmfmRuns` object created by `runExample()`.
#' @param method Character. One of "deterministic", "llm", or "both".
#' @param chat Optional chat provider object. If omitted and LLM scoring is
#'   requested, a provider is obtained via `getChatProvider()`.
#' @param useCache Logical. Passed to LLM scoring helpers.
#' @param showProgress Logical. Should progress be shown for LLM scoring?
#' @param verbose Logical. Passed to LLM scoring helpers.
#' @param ... Reserved for future method-specific arguments.
#'
#' @return An object of class `wmfmScores`.
#' @export
score.wmfmRuns = function(
    x,
    method = c("deterministic", "llm", "both"),
    chat = NULL,
    useCache = FALSE,
    showProgress = TRUE,
    verbose = FALSE,
    ...
) {

  method = match.arg(method)

  if (!inherits(x, "wmfmRuns")) {
    stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  methods =
    if (identical(method, "both")) {
      c("deterministic", "llm")
    } else {
      method
    }

  # Auto-resolve chat if needed
  if ("llm" %in% methods && is.null(chat)) {
    chat = tryCatch(
      getChatProvider(),
      error = function(e) {
        stop(
          "Could not get a chat provider for LLM scoring. Details: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  out = newWmfmScores(
    x = x,
    methods = methods
  )

  runRecords = x$runs

  if ("deterministic" %in% methods) {
    runsDf = do.call(
      rbind,
      lapply(runRecords, function(run) {
        as.data.frame(run, stringsAsFactors = FALSE)
      })
    )
    rownames(runsDf) = NULL

    detDf = scoreWmfmRepeatedRuns(runsDf)

    out$scores$deterministic = lapply(
      seq_len(nrow(detDf)),
      function(i) {
        result = extractWmfmScoreFields(detDf[i, , drop = FALSE])
        result$primaryScoringMethod = "deterministic"
        result
      }
    )
  }

  if ("llm" %in% methods) {
    llmScoredRuns = scoreWmfmRunsWithLlm(
      runRecords = runRecords,
      chat = chat,
      useCache = useCache,
      showProgress = showProgress,
      verbose = verbose
    )

    out$scores$llm = lapply(
      llmScoredRuns,
      function(run) {
        result = extractWmfmScoreFields(run)
        result$primaryScoringMethod = "llm"
        result
      }
    )
  }

  out$meta$scoredAt = as.character(Sys.time())
  out$meta$requestedMethod = method

  if ("llm" %in% methods) {
    out$meta$llmModel = safeWmfmScalar(class(chat)[1], naString = "")
  }

  out
}
