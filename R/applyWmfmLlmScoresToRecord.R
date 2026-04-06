#' Apply LLM scoring to a run record
#'
#' Wrapper around `scoreWmfmRunWithLlm()` that ensures consistent output
#' structure and attaches metadata if required.
#'
#' @param runRecord A single raw run record.
#' @param chat Chat provider.
#' @param useCache Logical.
#' @param verbose Logical.
#'
#' @return Named list containing scoring fields.
#' @export
applyWmfmLlmScoresToRecord = function(
    runRecord,
    chat,
    useCache = FALSE,
    verbose = FALSE
) {
  score = scoreWmfmRunWithLlm(
    runRecord = runRecord,
    chat = chat,
    useCache = useCache,
    verbose = verbose
  )

  score
}
