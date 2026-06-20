#' List packaged WMFM examples
#'
#' Lists packaged examples stored in the package's
#' `inst/extdata/examples` directory by looking for specification files
#' ending in `.spec.yml`.
#'
#' Each example is expected to live in its own subdirectory under
#' `extdata/examples/<name>/` and to contain a specification file named
#' `<name>.spec.yml`. Example visibility is controlled by the optional
#' `extdata/examples/example-metadata.yml` manifest.
#'
#' @param package A character string giving the package name containing the
#'   packaged examples.
#' @param includeTestExamples Logical. Should developer-only examples marked
#'   as `developer` or `test` in the example metadata be included? Defaults
#'   to `FALSE`.
#'
#' @return A character vector of available example names. If no examples are
#'   found, an empty character vector is returned.
#'
#' @examples
#' if (interactive()) {
#'   listWMFMExamples(package = "WMFM")
#' }
#'
#' @export
#' @importFrom stats as.formula
#' @importFrom tools file_ext Rd2txt
#' @importFrom utils read.csv read.table data capture.output
#' @importFrom yaml read_yaml
listWMFMExamples = function(package = "WMFM", includeTestExamples = FALSE) {
  exampleDetails = listWMFMExampleDetails(
    package = package,
    includeTestExamples = includeTestExamples
  )

  sort(unique(exampleDetails$exampleName))
}

#' List packaged WMFM example details
#'
#' Returns metadata-backed details for packaged examples stored under
#' `inst/extdata/examples`. This is intended for release-mode and
#' developer-mode example-library displays that need more than the example
#' names alone.
#'
#' @param package A character string giving the package name containing the
#'   packaged examples.
#' @param includeTestExamples Logical. Should developer-only examples marked
#'   as `developer` or `test` in the example metadata be included? Defaults
#'   to `FALSE`.
#'
#' @return A data frame with one row per available example and columns for
#'   example name, path, audience, model family, difficulty, teaching topic,
#'   and developer purpose.
#'
#' @examples
#' if (interactive()) {
#'   listWMFMExampleDetails(package = "WMFM")
#' }
#'
#' @export
listWMFMExampleDetails = function(package = "WMFM", includeTestExamples = FALSE) {
  examplesPath = system.file(
    "extdata",
    "examples",
    package = package
  )

  emptyDetails = getEmptyWMFMExampleRecords()

  if (examplesPath == "" || !dir.exists(examplesPath)) {
    return(emptyDetails)
  }

  specFiles = list.files(
    path = examplesPath,
    pattern = "\\.spec\\.yml$",
    recursive = TRUE,
    full.names = FALSE
  )

  if (length(specFiles) == 0) {
    return(emptyDetails)
  }

  exampleMetadata = loadWMFMExampleMetadata(examplesPath)
  exampleRecords = buildWMFMExampleRecords(
    examplesPath = examplesPath,
    specFiles = specFiles,
    exampleMetadata = exampleMetadata
  )

  if (!isTRUE(includeTestExamples)) {
    exampleRecords = exampleRecords[
      !exampleRecords$exampleAudience %in% c("developer", "test"),
      ,
      drop = FALSE
    ]
  }

  exampleRecords[order(exampleRecords$exampleName), , drop = FALSE]
}

#' Load packaged example metadata
#'
#' @param examplesPath Path to the installed examples directory.
#'
#' @return A named list of metadata records keyed by example folder name.
#'
#' @keywords internal
#' @noRd
loadWMFMExampleMetadata = function(examplesPath) {
  metadataPath = file.path(examplesPath, "example-metadata.yml")

  if (!file.exists(metadataPath)) {
    return(list())
  }

  metadata = read_yaml(metadataPath)

  if (is.null(metadata)) {
    return(list())
  }

  if (!is.list(metadata) || is.null(metadata$examples) || !is.list(metadata$examples)) {
    stop(
      "Example metadata file must contain an `examples` mapping.",
      call. = FALSE
    )
  }

  metadata$examples
}

#' Build packaged example records
#'
#' @param examplesPath Path to the installed examples directory.
#' @param specFiles Relative paths to packaged example specification files.
#' @param exampleMetadata Named list of example metadata records.
#'
#' @return A data frame with one row per example specification.
#'
#' @keywords internal
#' @noRd
buildWMFMExampleRecords = function(examplesPath, specFiles, exampleMetadata) {
  if (length(specFiles) == 0) {
    return(getEmptyWMFMExampleRecords())
  }

  records = lapply(
    specFiles,
    function(specFile) {
      specPath = file.path(examplesPath, specFile)
      spec = tryCatch(read_yaml(specPath), error = function(e) NULL)
      exampleDir = basename(dirname(specPath))
      metadata = exampleMetadata[[exampleDir]] %||% list()
      displayName = getWMFMExampleDisplayName(
        spec = spec,
        specFile = specFile,
        metadata = metadata
      )
      exampleAudience = getWMFMExampleAudience(
        exampleDir = exampleDir,
        metadata = metadata
      )

      data.frame(
        exampleDir = exampleDir,
        examplePath = dirname(specFile),
        specFile = specFile,
        exampleName = displayName,
        exampleAudience = exampleAudience,
        exampleFamily = getWMFMExampleMetadataText(metadata, "exampleFamily"),
        exampleDifficulty = getWMFMExampleMetadataText(metadata, "exampleDifficulty"),
        teachingTopic = getWMFMExampleMetadataText(metadata, "teachingTopic"),
        developerPurpose = getWMFMExampleMetadataText(metadata, "developerPurpose"),
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, records)
}

#' Get an empty packaged example records data frame
#'
#' @return A zero-row data frame with the packaged example detail columns.
#'
#' @keywords internal
#' @noRd
getEmptyWMFMExampleRecords = function() {
  data.frame(
    exampleDir = character(0),
    examplePath = character(0),
    specFile = character(0),
    exampleName = character(0),
    exampleAudience = character(0),
    exampleFamily = character(0),
    exampleDifficulty = character(0),
    teachingTopic = character(0),
    developerPurpose = character(0),
    stringsAsFactors = FALSE
  )
}

#' Get a single text field from packaged example metadata
#'
#' @param metadata Optional metadata record for the example.
#' @param fieldName Metadata field name to read.
#'
#' @return A single character string, or an empty string when absent.
#'
#' @keywords internal
#' @noRd
getWMFMExampleMetadataText = function(metadata, fieldName) {
  fieldValue = metadata[[fieldName]] %||% NULL

  if (is.character(fieldValue) && length(fieldValue) == 1 && !is.na(fieldValue)) {
    return(trimws(fieldValue))
  }

  ""
}

#' Get the display name for a packaged example
#'
#' @param spec Parsed example specification.
#' @param specFile Relative path to the example specification file.
#' @param metadata Optional metadata record for the example.
#'
#' @return A single example display name.
#'
#' @keywords internal
#' @noRd
getWMFMExampleDisplayName = function(spec, specFile, metadata) {
  displayName = metadata$displayName %||% NULL

  if (is.null(displayName) && is.list(spec)) {
    displayName = spec$displayName %||% NULL
  }

  if (is.character(displayName) && length(displayName) == 1 && !is.na(displayName)) {
    displayName = trimws(displayName)
  } else {
    displayName = ""
  }

  if (!nzchar(displayName)) {
    displayName = basename(sub("\\.spec\\.yml$", "", specFile))
  }

  displayName
}

#' Get the audience for a packaged example
#'
#' @param exampleDir Example directory name.
#' @param metadata Optional metadata record for the example.
#'
#' @return One of `public`, `classroom`, `developer`, or `test`.
#'
#' @keywords internal
#' @noRd
getWMFMExampleAudience = function(exampleDir, metadata) {
  exampleAudience = metadata$exampleAudience %||% NULL

  if (is.character(exampleAudience) && length(exampleAudience) == 1 && !is.na(exampleAudience)) {
    exampleAudience = trimws(exampleAudience)
  } else {
    exampleAudience = ""
  }

  if (!nzchar(exampleAudience)) {
    if (startsWith(exampleDir, "test")) {
      exampleAudience = "test"
    } else {
      exampleAudience = "public"
    }
  }

  validAudiences = c("public", "classroom", "developer", "test")

  if (!exampleAudience %in% validAudiences) {
    stop(
      "Unsupported example audience `",
      exampleAudience,
      "` for example `",
      exampleDir,
      "`.",
      call. = FALSE
    )
  }

  exampleAudience
}


#' Find a packaged example record by display name, stem, or directory
#'
#' @param requestedName User-supplied example name.
#' @param exampleRecords Data frame returned by `buildWMFMExampleRecords()`.
#'
#' @return A one-row data frame for the matched example, or `NULL`.
#'
#' @keywords internal
#' @noRd
findWMFMExampleRecord = function(requestedName, exampleRecords) {
  if (nrow(exampleRecords) == 0) {
    return(NULL)
  }

  exampleStem = sub("\\.spec\\.yml$", "", basename(exampleRecords$specFile))
  matched = which(
    requestedName == exampleRecords$exampleName |
      requestedName == exampleStem |
      requestedName == exampleRecords$exampleDir
  )

  if (length(matched) == 0) {
    return(NULL)
  }

  exampleRecords[matched[1], , drop = FALSE]
}


#' Format packaged example metadata for developer UI display
#'
#' @param selectedName Character scalar selected example display name, stem, or
#'   directory name.
#' @param package Character. Package containing the examples. Defaults to
#'   `"WMFM"`.
#' @param includeTestExamples Logical. Should developer-only examples be searched?
#'
#' @return A character vector of labelled metadata lines for the selected
#'   example.
#'
#' @keywords internal
#' @noRd
formatWMFMExampleMetadataLines = function(
  selectedName,
  package = "WMFM",
  includeTestExamples = TRUE
) {
  requestedName = trimws(as.character(selectedName %||% ""))

  if (!nzchar(requestedName)) {
    return("Choose an example to see developer metadata.")
  }

  exampleDetails = listWMFMExampleDetails(
    package = package,
    includeTestExamples = includeTestExamples
  )
  matchedRecord = findWMFMExampleRecord(
    requestedName = requestedName,
    exampleRecords = exampleDetails
  )

  if (is.null(matchedRecord)) {
    return(paste0("No metadata found for example: ", requestedName))
  }

  metadataRows = c(
    paste0("Example: ", matchedRecord$exampleName),
    paste0("Audience: ", matchedRecord$exampleAudience),
    paste0("Family: ", formatWMFMExampleMetadataValue(matchedRecord$exampleFamily)),
    paste0("Difficulty: ", formatWMFMExampleMetadataValue(matchedRecord$exampleDifficulty)),
    paste0("Teaching topic: ", formatWMFMExampleMetadataValue(matchedRecord$teachingTopic)),
    paste0("Developer purpose: ", formatWMFMExampleMetadataValue(matchedRecord$developerPurpose)),
    paste0("Path: inst/extdata/examples/", matchedRecord$examplePath),
    paste0("Spec: ", matchedRecord$specFile)
  )

  metadataRows
}

#' Format a scalar example metadata value for display
#'
#' @param value Metadata value.
#'
#' @return A single non-empty character string.
#'
#' @keywords internal
#' @noRd
formatWMFMExampleMetadataValue = function(value) {
  valueText = trimws(as.character(value %||% ""))

  if (!nzchar(valueText) || is.na(valueText)) {
    return("not recorded")
  }

  valueText
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
#'   \item{researchQuestion}{Optional research question text, or `NULL`.}
#'   \item{followupQuestion}{Optional follow-up question text, or `NULL`.}
#' }
#'
#' @keywords internal
#' @noRd
loadExampleSpec = function(name, package = "WMFM") {
  examplesPath = system.file("extdata", "examples", package = package)
  if (examplesPath == "" || !dir.exists(examplesPath)) {
    stop("Example not found: ", name, call. = FALSE)
  }

  requestedName = trimws(as.character(name %||% ""))
  if (!nzchar(requestedName)) {
    stop("Example name must be a non-empty string.", call. = FALSE)
  }

  specFiles = list.files(
    path = examplesPath,
    pattern = "\\.spec\\.yml$",
    recursive = TRUE,
    full.names = FALSE
  )
  exampleMetadata = loadWMFMExampleMetadata(examplesPath)
  exampleRecords = buildWMFMExampleRecords(
    examplesPath = examplesPath,
    specFiles = specFiles,
    exampleMetadata = exampleMetadata
  )

  matchedRecord = findWMFMExampleRecord(
    requestedName = requestedName,
    exampleRecords = exampleRecords
  )

  if (is.null(matchedRecord)) {
    availableExamples = listWMFMExamples(package = package)

    if (length(availableExamples) == 0) {
      stop("Example not found: ", name, call. = FALSE)
    }

    stop(
      "Example not found: ",
      requestedName,
      ". Available examples are: ",
      paste(availableExamples, collapse = ", "),
      call. = FALSE
    )
  }

  specPath = file.path(examplesPath, matchedRecord$specFile)
  basePath = file.path(examplesPath, matchedRecord$examplePath)
  specFileName = basename(specPath)

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

  researchQuestion = NULL
  followupQuestion = NULL

  if (!is.null(spec$researchQuestion)) {
    if (!is.character(spec$researchQuestion) ||
        length(spec$researchQuestion) != 1 ||
        is.na(spec$researchQuestion)) {
      stop(
        "If supplied, `researchQuestion` must be a single non-missing character string.",
        call. = FALSE
      )
    }

    researchQuestion = trimws(spec$researchQuestion)

    if (!nzchar(researchQuestion)) {
      researchQuestion = NULL
    }
  }

  if (!is.null(spec$followupQuestion)) {
    if (!is.character(spec$followupQuestion) ||
        length(spec$followupQuestion) != 1 ||
        is.na(spec$followupQuestion)) {
      stop(
        "If supplied, `followupQuestion` must be a single non-missing character string.",
        call. = FALSE
      )
    }

    followupQuestion = trimws(spec$followupQuestion)

    if (!nzchar(followupQuestion)) {
      followupQuestion = NULL
    }
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
    dataContext = dataContext,
    researchQuestion = researchQuestion,
    followupQuestion = followupQuestion
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

    data = get(spec$dataObject, envir = loadEnv, inherits = FALSE)
    return(applyWMFMExampleDataTransform(data = data, spec = spec))
  }

  stop(
    "Unsupported `dataSource`: ",
    dataSource,
    ". Supported values are `file` and `package`.",
    call. = FALSE
  )
}


#' Apply an optional packaged-example data transform
#'
#' Applies small, named compatibility transforms used by packaged examples.
#' The raw package dataset remains the source of truth; transforms only add
#' aliases needed by stable example specifications.
#'
#' @param data A data object loaded from an example source.
#' @param spec A list read from the example specification file.
#'
#' @return The possibly transformed data object.
#'
#' @keywords internal
#' @noRd
applyWMFMExampleDataTransform = function(data, spec) {
  transformName = spec$dataTransform

  if (is.null(transformName) || !nzchar(transformName)) {
    return(data)
  }

  if (identical(transformName, "courseDfScoringGradingAliases")) {
    return(addCourseDfScoringGradingAliases(data))
  }

  if (identical(transformName, "diamondsPlainFactors")) {
    return(convertDiamondsOrderedFactors(data))
  }

  stop(
    "Unsupported `dataTransform`: ",
    transformName,
    call. = FALSE
  )
}

#' Add stable scoring-and-grading aliases to s20x course.df
#'
#' Adds the historical Stage 18 scoring variable names to `s20x::course.df`
#' without changing the source rows. This lets the SG examples use the package
#' dataset while retaining their original formula names.
#'
#' @param data A data frame loaded from `s20x::course.df`.
#'
#' @return A data frame with added alias columns where needed.
#'
#' @keywords internal
#' @noRd
addCourseDfScoringGradingAliases = function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  if ("Assign" %in% names(data) && !("Assignment" %in% names(data))) {
    data$Assignment = data$Assign
  }

  if ("Test" %in% names(data) && !("StudyHours" %in% names(data))) {
    data$StudyHours = data$Test
  }

  data
}


#' Convert ggplot2 diamonds ordered factors to ordinary factors
#'
#' The `ggplot2::diamonds` variables `cut`, `color`, and `clarity` are
#' ordered factors. WMFM examples use these as ordinary categorical adjustment
#' variables so that fitted-model summaries use treatment contrasts rather than
#' orthogonal polynomial contrasts.
#'
#' @param data A data frame loaded from `ggplot2::diamonds`.
#'
#' @return A data frame with selected ordered factors converted to ordinary
#'   factors.
#'
#' @keywords internal
#' @noRd
convertDiamondsOrderedFactors = function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  factorNames = intersect(c("cut", "color", "clarity"), names(data))

  for (factorName in factorNames) {
    data[[factorName]] = factor(
      data[[factorName]],
      levels = levels(data[[factorName]]),
      ordered = FALSE
    )
  }

  data
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
#'   \item{researchQuestion}{Optional example research question text.}
#'   \item{runs}{List of raw run records.}
#'   \item{meta}{Metadata about the run set, including elapsed time, average
#'   per-run time, and per-run timing details.}
#' }
#'
#' @examples
#' if (interactive()) {
#'   x = runExample("Course")
#'   y = runExample("Course", nRuns = 20)
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

  adjustmentVariables = exampleInfo$spec$adjustmentVariables %||% character(0)
  primaryVariables = exampleInfo$spec$primaryVariables %||% character(0)

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
        researchQuestion = exampleInfo$researchQuestion,
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
      interactionAlpha = interactionAlpha,
      adjustmentVariables = adjustmentVariables,
      primaryVariables = primaryVariables
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
    researchQuestion = exampleInfo$researchQuestion,
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
