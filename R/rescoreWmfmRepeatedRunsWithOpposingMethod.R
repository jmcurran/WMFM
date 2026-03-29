#' Rescore repeated WMFM runs with the opposing method
#'
#' Takes an existing repeated-run object and scores each run again with the
#' opposite of its primary scoring method. The newly computed scores are stored
#' alongside the existing scores using either a `"det_"` or `"llm_"` prefix.
#'
#' @param x Object produced by `runWMFMPackageExampleRepeated()`.
#' @param scoringChat Optional chat provider object used when rescoring with the
#'   LLM.
#' @param useScoringCache Logical. Passed to `scoreWmfmRunWithLlm()` when the
#'   opposing method is `"llm"`.
#' @param verbose Logical. Passed to `scoreWmfmRunWithLlm()` when the opposing
#'   method is `"llm"`.
#' @param showProgress Logical. Should a console progress bar be shown?
#'
#' @return The input object with additional prefixed columns in `runsDf` and an
#'   `opposingScoringMethod` element.
#' @export
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
rescoreWmfmRepeatedRunsWithOpposingMethod = function(
    x,
    scoringChat = NULL,
    useScoringCache = FALSE,
    verbose = FALSE,
    showProgress = TRUE
) {

  if (!is.list(x) || is.null(x$runsDf) || !is.data.frame(x$runsDf)) {
    stop("`x` must contain a `runsDf` data frame.", call. = FALSE)
  }

  primaryMethod = x$primaryScoringMethod %||% NA_character_

  if (!primaryMethod %in% c("deterministic", "llm")) {
    stop(
      "Opposing rescoring requires `x$primaryScoringMethod` to be either ",
      "\"deterministic\" or \"llm\".",
      call. = FALSE
    )
  }

  opposingMethod = if (identical(primaryMethod, "deterministic")) "llm" else "deterministic"
  opposingPrefix = if (identical(opposingMethod, "llm")) "llm_" else "det_"

  if (identical(opposingMethod, "llm") && is.null(scoringChat)) {
    stop(
      "`scoringChat` must be supplied when the opposing method is `\"llm\"`.",
      call. = FALSE
    )
  }

  nRuns = nrow(x$runsDf)
  scoredList = vector("list", nRuns)

  pb = NULL
  if (isTRUE(showProgress) && interactive() && nRuns > 1) {
    pb = utils::txtProgressBar(min = 0, max = nRuns, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in seq_len(nRuns)) {
    runRecord = as.list(x$runsDf[i, , drop = FALSE])

    scoredList[[i]] = scoreWmfmRunRecordByMethod(
      runRecord = runRecord,
      scoringMethod = opposingMethod,
      scoringChat = scoringChat,
      useScoringCache = useScoringCache,
      verbose = verbose
    )

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  scoredDf = do.call(rbind, lapply(scoredList, as.data.frame))
  rownames(scoredDf) = NULL

  compareFields = c(
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
    "llmScored",
    "llmScoringModel",
    "llmScoringRaw",
    "llmScoringSummary",
    "llmFieldReasons"
  )

  keepFields = intersect(compareFields, names(scoredDf))

  for (field in keepFields) {
    x$runsDf[[paste0(opposingPrefix, field)]] = scoredDf[[field]]
  }

  x$opposingScoringMethod = opposingMethod
  x
}
