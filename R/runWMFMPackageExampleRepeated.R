#' Run a packaged WMFM example multiple times and collate outputs
#'
#' Loads a stored example from the package's `inst/extdata/examples`
#' directory, reads its specification, data, and optional context, and then
#' runs `runWMFMModelDebug()` repeatedly using those same inputs.
#'
#' This is useful for checking response stability across repeated calls to the
#' language model for the same fitted model. The function collects the raw
#' equations and explanation outputs from each run and computes simple text
#' features and semantic claim fields that help compare the explanations.
#'
#' A console progress bar is displayed by default. After each run, the function
#' updates the estimated time remaining using the average elapsed time per
#' completed run.
#'
#' By default, explanation caching is disabled in repeated-run mode so that
#' each repetition makes a fresh explanation request to the language model.
#'
#' @param name Character. Name of the example folder.
#' @param package Character. Package name.
#' @param nRuns Integer. Number of repetitions.
#' @param printOutput Logical. Passed to `runWMFMModelDebug()`.
#' @param pauseSeconds Numeric. Optional delay between runs.
#' @param showProgress Logical. Should a console progress bar and estimated
#'   time remaining be shown?
#' @param useExplanationCache Logical. Passed to `runWMFMModelDebug()`.
#'   Defaults to `FALSE` so repeated runs query the language model each time.
#' @param interactionAlpha Numeric. Threshold used when judging whether an
#'   explanation interpreted interaction evidence appropriately.
#' @param ... Additional arguments passed to `runWMFMModelDebug()`.
#'
#' @return A list with elements:
#' \describe{
#'   \item{runsDf}{Data frame of all runs}
#'   \item{summary}{Summary statistics}
#'   \item{spec}{Specification}
#'   \item{dataContext}{Context text}
#' }
#' @export
runWMFMPackageExampleRepeated = function(
    name,
    package = "WMFM",
    nRuns = 10,
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

  if (!is.numeric(pauseSeconds) || length(pauseSeconds) != 1 || is.na(pauseSeconds) || pauseSeconds < 0) {
    stop("`pauseSeconds` must be a single non-negative number.", call. = FALSE)
  }

  if (!is.logical(showProgress) || length(showProgress) != 1 || is.na(showProgress)) {
    stop("`showProgress` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(useExplanationCache) || length(useExplanationCache) != 1 || is.na(useExplanationCache)) {
    stop("`useExplanationCache` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(interactionAlpha) || length(interactionAlpha) != 1 || is.na(interactionAlpha) || interactionAlpha <= 0 || interactionAlpha >= 1) {
    stop("`interactionAlpha` must be a single number strictly between 0 and 1.", call. = FALSE)
  }

  basePath = system.file("extdata", "examples", name, package = package)

  if (basePath == "") {
    stop("Example not found: ", name, call. = FALSE)
  }

  specPath = file.path(basePath, paste0(name, ".spec.yml"))
  spec = yaml::read_yaml(specPath)

  data = loadWMFMExampleData(spec = spec, basePath = basePath)
  dataContext = loadWMFMExampleContext(spec = spec, basePath = basePath)

  runResults = vector("list", nRuns)

  progressBar = NULL
  overallStartTime = Sys.time()

  if (showProgress) {
    progressBar = utils::txtProgressBar(
      min = 0,
      max = nRuns,
      initial = 0,
      style = 3
    )
    on.exit(close(progressBar), add = TRUE)
    message("Starting repeated WMFM example run...")
  }

  for (i in seq_len(nRuns)) {

    result = tryCatch(
      runWMFMModelDebug(
        data = data,
        formula = stats::as.formula(spec$formula),
        modelType = spec$modelType,
        dataContext = dataContext,
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

    runResults[[i]] = buildWmfmRunRecord(
      runId = i,
      exampleName = name,
      package = package,
      modelType = spec$modelType,
      formula = spec$formula,
      equationsText = extractWmfmText(result$equations),
      explanationText = extractWmfmText(result$explanation),
      errorMessage = result$.error %||% NA_character_,
      interactionTerms = result$interactionTerms %||% character(0),
      interactionMinPValue = result$interactionMinPValue %||% NA_real_,
      interactionAlpha = interactionAlpha
    )

    if (pauseSeconds > 0) {
      Sys.sleep(pauseSeconds)
    }

    if (showProgress) {
      utils::setTxtProgressBar(progressBar, i)

      elapsedSeconds = as.numeric(difftime(Sys.time(), overallStartTime, units = "secs"))
      avgSecondsPerRun = elapsedSeconds / i
      remainingRuns = nRuns - i
      remainingSeconds = avgSecondsPerRun * remainingRuns

      message(
        sprintf(
          " Completed %d/%d runs | Elapsed: %s | Estimated time remaining: %s",
          i,
          nRuns,
          formatWmfmElapsedTime(elapsedSeconds),
          formatWmfmElapsedTime(remainingSeconds)
        )
      )
    }
  }

  runsDf = do.call(rbind, lapply(runResults, as.data.frame))
  rownames(runsDf) = NULL

  summary = summariseWmfmRepeatedRuns(runsDf)

  if (showProgress) {
    totalSeconds = as.numeric(difftime(Sys.time(), overallStartTime, units = "secs"))
    message("Finished. Total elapsed time: ", formatWmfmElapsedTime(totalSeconds))
  }

  list(
    runsDf = runsDf,
    summary = summary,
    spec = spec,
    dataContext = dataContext
  )
}
