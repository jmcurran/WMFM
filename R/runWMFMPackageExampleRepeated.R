#' Run a packaged WMFM example multiple times and collate outputs
#'
#' Loads a stored example from the package's `inst/extdata/examples`
#' directory, reads its specification, data, and optional context, and then
#' runs `runWMFMModelDebug()` repeatedly using those same inputs.
#'
#' This is useful for checking response stability across repeated calls to the
#' language model for the same fitted model. The function collects the raw
#' equations and explanation outputs from each run and computes simple text
#' features that help compare the explanations.
#'
#' @param name Character. Name of the example folder.
#' @param package Character. Package name.
#' @param nRuns Integer. Number of repetitions.
#' @param printOutput Logical. Passed to `runWMFMModelDebug()`.
#' @param pauseSeconds Numeric. Optional delay between runs.
#' @param ... Additional arguments passed to `runWMFMModelDebug()`.
#'
#' @return A list with elements:
#' \describe{
#'   \item{runsDf}{Data frame of all runs}
#'   \item{summary}{Summary statistics}
#'   \item{spec}{Specification}
#'   \item{dataContext}{Context text}
#' }
#'
#' @export
runWMFMPackageExampleRepeated = function(
    name,
    package = "WMFM",
    nRuns = 10,
    printOutput = FALSE,
    pauseSeconds = 0,
    ...
) {

  nRuns = as.integer(nRuns)

  basePath = system.file("extdata", "examples", name, package = package)

  if (basePath == "") {
    stop("Example not found: ", name, call. = FALSE)
  }

  specPath = file.path(basePath, paste0(name, ".spec.yml"))
  spec = yaml::read_yaml(specPath)

  data = loadWMFMExampleData(spec = spec, basePath = basePath)
  dataContext = loadWMFMExampleContext(spec = spec, basePath = basePath)

  runResults = vector("list", nRuns)

  for (i in seq_len(nRuns)) {

    result = tryCatch(
      runWMFMModelDebug(
        data = data,
        formula = stats::as.formula(spec$formula),
        modelType = spec$modelType,
        dataContext = dataContext,
        printOutput = printOutput,
        ...
      ),
      error = function(e) {
        list(explanation = NULL, equations = NULL, .error = conditionMessage(e))
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
      errorMessage = result$.error %||% NA_character_
    )

    if (pauseSeconds > 0) {
      Sys.sleep(pauseSeconds)
    }
  }

  runsDf = do.call(rbind, lapply(runResults, as.data.frame))
  rownames(runsDf) = NULL

  summary = summariseWmfmRepeatedRuns(runsDf)

  list(
    runsDf = runsDf,
    summary = summary,
    spec = spec,
    dataContext = dataContext
  )
}
