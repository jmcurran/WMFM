#' List packaged WMFM examples
#'
#' Lists packaged examples stored in the package's
#' `inst/extdata/examples` directory by looking for specification files
#' ending in `.spec.yml`.
#'
#' Each example is expected to live in its own subdirectory under
#' `extdata/examples/<name>/` and to contain a specification file named
#' `<name>.spec.yml`.
#'
#' @param package A character string giving the package name containing the
#'   packaged examples.
#'
#' @return A character vector of available example names. If no examples are
#'   found, an empty character vector is returned.
#'
#' @examples
#' \dontrun{
#' listWMFMExamples(package = "WMFM")
#' }
#'
#' @export
#' @importFrom stats as.formula
#' @importFrom tools file_ext Rd2txt
#' @importFrom utils read.csv read.table data capture.output
#' @importFrom yaml read_yaml
listWMFMExamples = function(package = "WMFM") {
  examplesPath = system.file(
    "extdata",
    "examples",
    package = package
  )

  if (examplesPath == "" || !dir.exists(examplesPath)) {
    return(character(0))
  }

  specFiles = list.files(
    path = examplesPath,
    pattern = "\\.spec\\.yml$",
    recursive = TRUE,
    full.names = FALSE
  )

  if (length(specFiles) == 0) {
    return(character(0))
  }

  exampleNames = basename(specFiles)
  exampleNames = sub("\\.spec\\.yml$", "", exampleNames)

  sort(unique(exampleNames))
}

#' Load packaged WMFM example inputs
#'
#' Loads and validates the specification, data, and optional context for a
#' packaged WMFM example.
#'
#' @param name Character. Name of the example folder under
#'   `inst/extdata/examples`.
#' @param package Character. Package containing the example. Defaults to
#'   `"WMFM"`.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{basePath}{Path to the example folder.}
#'   \item{spec}{Parsed example specification.}
#'   \item{data}{Loaded example data.}
#'   \item{dataContext}{Optional example context text, or `NULL`.}
#' }
#'
#' @keywords internal
#' @noRd
loadExampleSpec = function(name, package = "WMFM") {
  basePath = system.file(
    "extdata",
    "examples",
    name,
    package = package
  )

  if (basePath == "") {
    availableExamples = listWMFMExamples(package = package)

    if (length(availableExamples) == 0) {
      stop("Example not found: ", name, call. = FALSE)
    }

    stop(
      "Example not found: ",
      name,
      ". Available examples are: ",
      paste(availableExamples, collapse = ", "),
      call. = FALSE
    )
  }

  specFileName = paste0(name, ".spec.yml")
  specPath = file.path(basePath, specFileName)

  if (!file.exists(specPath)) {
    stop(
      "Missing specification file: ",
      specFileName,
      " in example: ",
      name,
      call. = FALSE
    )
  }

  spec = read_yaml(specPath)

  if (is.null(spec$formula) || !nzchar(spec$formula)) {
    stop(
      "Specification file must contain a non-empty `formula` entry.",
      call. = FALSE
    )
  }

  if (is.null(spec$modelType) || !nzchar(spec$modelType)) {
    stop(
      "Specification file must contain a non-empty `modelType` entry.",
      call. = FALSE
    )
  }

  data = loadWMFMExampleData(spec = spec, basePath = basePath)

  if (!is.data.frame(data)) {
    stop("Loaded example data is not a data.frame.", call. = FALSE)
  }

  dataContext = loadWMFMExampleContext(spec = spec, basePath = basePath)

  list(
    basePath = basePath,
    spec = spec,
    data = data,
    dataContext = dataContext
  )
}

#' Load example data for a packaged WMFM example
#'
#' Loads example data according to the specification for a packaged WMFM
#' example.
#'
#' Supported data sources are file-based data bundled with the example and
#' package-based datasets from installed packages.
#'
#' @param spec A list read from the example specification file.
#' @param basePath A character string giving the path to the example folder.
#'
#' @return A dataset object. In normal use this should be a `data.frame`.
#'
#' @keywords internal
#' @noRd
loadWMFMExampleData = function(spec, basePath) {
  dataSource = spec$dataSource

  if (is.null(dataSource) || !nzchar(dataSource)) {
    dataSource = "file"
  }

  if (identical(dataSource, "file")) {
    if (is.null(spec$data) || !nzchar(spec$data)) {
      stop(
        "For file-based examples, the specification must contain a non-empty `data` entry.",
        call. = FALSE
      )
    }

    dataPath = file.path(basePath, spec$data)

    if (!file.exists(dataPath)) {
      stop("Data file not found: ", dataPath, call. = FALSE)
    }

    extension = tolower(file_ext(dataPath))

    if (identical(extension, "csv")) {
      return(read.csv(dataPath, stringsAsFactors = FALSE))
    }

    if (identical(extension, "txt")) {
      return(read.table(dataPath, header = TRUE, stringsAsFactors = FALSE))
    }

    if (extension %in% c("rda", "rdata")) {
      loadEnv = new.env(parent = emptyenv())
      objectNames = load(dataPath, envir = loadEnv)

      if (length(objectNames) == 0) {
        stop("No objects found in ", dataPath, call. = FALSE)
      }

      if (length(objectNames) > 1) {
        warning(
          "Multiple objects found in ",
          dataPath,
          ". Using the first: ",
          objectNames[1],
          call. = FALSE
        )
      }

      return(loadEnv[[objectNames[1]]])
    }

    stop(
      "Unsupported data file type: .",
      extension,
      ". Supported file types are csv, txt, rda, and RData.",
      call. = FALSE
    )
  }

  if (identical(dataSource, "package")) {
    if (is.null(spec$dataPackage) || !nzchar(spec$dataPackage)) {
      stop(
        "For package-based examples, the specification must contain a non-empty `dataPackage` entry.",
        call. = FALSE
      )
    }

    if (is.null(spec$dataObject) || !nzchar(spec$dataObject)) {
      stop(
        "For package-based examples, the specification must contain a non-empty `dataObject` entry.",
        call. = FALSE
      )
    }

    if (!requireNamespace(spec$dataPackage, quietly = TRUE)) {
      stop(
        "Required package `",
        spec$dataPackage,
        "` is not installed.",
        call. = FALSE
      )
    }

    loadEnv = new.env(parent = emptyenv())

    data(
      list = spec$dataObject,
      package = spec$dataPackage,
      envir = loadEnv
    )

    if (!exists(spec$dataObject, envir = loadEnv, inherits = FALSE)) {
      stop(
        "Dataset `",
        spec$dataObject,
        "` could not be loaded from package `",
        spec$dataPackage,
        "`.",
        call. = FALSE
      )
    }

    return(get(spec$dataObject, envir = loadEnv, inherits = FALSE))
  }

  stop(
    "Unsupported `dataSource`: ",
    dataSource,
    ". Supported values are `file` and `package`.",
    call. = FALSE
  )
}

#' Load example context for a packaged WMFM example
#'
#' Loads example context either from a file named in the example
#' specification or, for package-based datasets, from the dataset's
#' associated package help page.
#'
#' @param spec A list read from the example specification file.
#' @param basePath A character string giving the path to the example folder.
#'
#' @return A character string containing the example context, or `NULL` if no
#'   context could be loaded.
#'
#' @keywords internal
#' @noRd
loadWMFMExampleContext = function(spec, basePath) {
  if (!is.null(spec$context) && nzchar(spec$context)) {
    contextPath = file.path(basePath, spec$context)

    if (!file.exists(contextPath)) {
      stop("Context file not found: ", contextPath, call. = FALSE)
    }

    return(
      paste(
        readLines(contextPath, warn = FALSE),
        collapse = "\n"
      )
    )
  }

  dataSource = spec$dataSource

  if (is.null(dataSource) || !nzchar(dataSource)) {
    dataSource = "file"
  }

  if (!identical(dataSource, "package")) {
    return(NULL)
  }

  if (is.null(spec$dataPackage) || !nzchar(spec$dataPackage)) {
    return(NULL)
  }

  if (is.null(spec$dataObject) || !nzchar(spec$dataObject)) {
    return(NULL)
  }

  rd = getInstalledPackageRd(
    topic = spec$dataObject,
    package = spec$dataPackage
  )

  if (is.null(rd)) {
    return(NULL)
  }

  helpText = tryCatch(
    paste(capture.output(Rd2txt(rd, out = "")), collapse = "\n"),
    error = function(e) {
      NULL
    }
  )

  helpText
}

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
        formula = as.formula(exampleInfo$spec$formula),
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
