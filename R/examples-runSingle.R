#' Run a packaged WMFM example one or more times
#'
#' Loads a packaged example, fits the specified model, generates equations and a
#' plain-language explanation, and stores each run as a separate raw run record.
#'
#' This function is a unified replacement for separate single-run and
#' repeated-run entry points. Use `nRuns = 1` for a single run.
#'
#' No scoring is performed here. Scoring should be done later using `score()`
#' on the returned object.
#'
#' @param name Character. Name of the packaged example.
#' @param package Character. Package containing the example. Defaults to
#'   `"WMFM"`.
#' @param nRuns Integer. Number of runs to perform. Defaults to `1`.
#' @param printOutput Logical. Passed to `runModel()`.
#' @param pauseSeconds Numeric. Optional delay between runs.
#' @param showProgress Logical. Should a console progress bar and timing summary
#'   be shown when work is repeated?
#' @param useExplanationCache Logical. Passed to `runModel()`.
#'   Defaults to `FALSE` so repeated runs will usually query the LLM afresh.
#' @param interactionAlpha Numeric. Threshold used when judging whether
#'   interaction evidence wording is appropriate in `buildWmfmRunRecord()`.
#' @param ... Additional arguments passed to `runModel()`.
#'
#' @return An object of class `wmfmRuns` with elements:
#' \describe{
#'   \item{exampleName}{Example name.}
#'   \item{package}{Package name.}
#'   \item{spec}{Parsed example specification.}
#'   \item{dataContext}{Optional example context text.}
#'   \item{runs}{List of raw run records.}
#'   \item{meta}{Metadata about the run set, including elapsed time, average
#'   per-run time, and per-run timing details.}
#' }
#'
#' @examples
#' \dontrun{
#' x = runExample("Course")
#' y = runExample("Course", nRuns = 20)
#' }
#'
#' @export
runExample = function(
    name,
    package = "WMFM",
    nRuns = 1,
    printOutput = FALSE,
    pauseSeconds = 0,
    showProgress = TRUE,
    useExplanationCache = FALSE,
    interactionAlpha = 0.05,
    ...
) {

  nRuns = as.integer(nRuns)

  if (length(nRuns) != 1 || is.na(nRuns) || nRuns < 1) {
    stop("`nRuns` must be a single positive integer.", call. = FALSE)
  }

  if (!is.logical(printOutput) || length(printOutput) != 1 || is.na(printOutput)) {
    stop("`printOutput` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(pauseSeconds) || length(pauseSeconds) != 1 || is.na(pauseSeconds) || pauseSeconds < 0) {
    stop("`pauseSeconds` must be a single non-negative number.", call. = FALSE)
  }

  if (!is.logical(showProgress) || length(showProgress) != 1 || is.na(showProgress)) {
    stop("`showProgress` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(useExplanationCache) || length(useExplanationCache) != 1 || is.na(useExplanationCache)) {
    stop("`useExplanationCache` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(interactionAlpha) || length(interactionAlpha) != 1 || is.na(interactionAlpha) ||
      interactionAlpha <= 0 || interactionAlpha >= 1) {
    stop(
      "`interactionAlpha` must be a single number strictly between 0 and 1.",
      call. = FALSE
    )
  }

  exampleInfo = loadExampleSpec(
    name = name,
    package = package
  )

  runs = vector("list", nRuns)
  tracker = newWmfmProgressTracker(
    nSteps = nRuns,
    showProgress = showProgress,
    label = "Run"
  )
  on.exit(closeWmfmProgressTracker(tracker), add = TRUE)

  for (i in seq_len(nRuns)) {
    iterationStartTime = Sys.time()

    result = tryCatch(
      runModel(
        data = exampleInfo$data,
        formula = stats::as.formula(exampleInfo$spec$formula),
        modelType = exampleInfo$spec$modelType,
        dataContext = exampleInfo$dataContext,
        printOutput = printOutput,
        useExplanationCache = useExplanationCache,
        ...
      ),
      error = function(e) {
        list(
          explanation = NULL,
          equations = NULL,
          interactionTerms = character(0),
          interactionMinPValue = NA_real_,
          .error = conditionMessage(e)
        )
      }
    )

    iterationSeconds = as.numeric(
      difftime(Sys.time(), iterationStartTime, units = "secs")
    )

    runs[[i]] = buildWmfmRunRecord(
      runId = i,
      exampleName = name,
      package = package,
      modelType = exampleInfo$spec$modelType,
      formula = exampleInfo$spec$formula,
      equationsText = extractWmfmText(result$equations),
      explanationText = extractWmfmText(result$explanation),
      errorMessage = result$.error %||% NA_character_,
      interactionTerms = result$interactionTerms %||% character(0),
      interactionMinPValue = result$interactionMinPValue %||% NA_real_,
      interactionAlpha = interactionAlpha
    )

    runs[[i]]$runElapsedSeconds = iterationSeconds

    if (isTRUE(showProgress)) {
      updateWmfmProgressTracker(
        tracker = tracker,
        step = i,
        stepSeconds = iterationSeconds
      )
    }

    if (pauseSeconds > 0 && i < nRuns) {
      Sys.sleep(pauseSeconds)
    }
  }

  timing = closeWmfmProgressTracker(tracker)

  out = list(
    exampleName = name,
    package = package,
    spec = exampleInfo$spec,
    dataContext = exampleInfo$dataContext,
    runs = runs,
    meta = list(
      nRuns = nRuns,
      createdAt = timing$finishedAt,
      startedAt = timing$startedAt,
      elapsedSeconds = timing$elapsedSeconds,
      averageRunSeconds = timing$averageIterationSeconds,
      runSeconds = timing$iterationSeconds,
      useExplanationCache = useExplanationCache,
      interactionAlpha = interactionAlpha
    )
  )

  class(out) = c("wmfmRuns", class(out))
  out
}
