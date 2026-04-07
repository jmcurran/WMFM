#' List packaged WMFM examples
#'
#' Lists packaged examples stored in the package's `inst/extdata/examples`
#' directory by looking for specification files ending in `.spec.yml`.
#'
#' Each example is expected to live in its own subdirectory under
#' `extdata/examples/<name>/` and to contain a specification file named
#' `<name>.spec.yml`.
#'
#' @param package A character string giving the package name.
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

#' Run a packaged WMFM example
#'
#' Loads a stored example from the package's `inst/extdata/examples`
#' directory, reads its specification, data, and optional context, and then
#' runs `runModel()` using those inputs.
#'
#' Each example is expected to live in its own subdirectory under
#' `extdata/examples/<name>/` and to contain a specification file named
#' `<name>.spec.yml`.
#'
#' Supported data sources are:
#' \describe{
#'   \item{`file`}{A data file bundled with the example directory. Supported
#'   formats are `.csv`, `.txt`, `.rda`, and `.RData`.}
#'   \item{`package`}{A dataset loaded from another installed package, such as
#'   `s20x`.}
#' }
#'
#' For package-based data sources, if no explicit context file is supplied in
#' the specification, the function attempts to use the associated package help
#' page as the data context.
#'
#' @param name A character string giving the name of the example folder under
#'   `inst/extdata/examples`. The specification file is assumed to be named
#'   `<name>.spec.yml`.
#' @param package A character string giving the package name containing the
#'   packaged examples.
#' @param printOutput Logical. Passed to `runModel()`. If `TRUE`,
#'   prints the model summary, fitted equations, and explanation.
#' @param ... Additional arguments passed on to `runModel()`.
#'
#' @return Invisibly returns the result of `runModel()`.
#'
#' @examples
#' \dontrun{
#' runWMFMPackageExample("Course", package = "WMFM")
#' }
#'
#' @export
runWMFMPackageExample = function(
  name,
  package = "WMFM",
  printOutput = TRUE,
  ...
) {
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

  spec = yaml::read_yaml(specPath)

  if (is.null(spec$formula) || !nzchar(spec$formula)) {
    stop("Specification file must contain a non-empty `formula` entry.", call. = FALSE)
  }

  if (is.null(spec$modelType) || !nzchar(spec$modelType)) {
    stop("Specification file must contain a non-empty `modelType` entry.", call. = FALSE)
  }

  data = loadWMFMExampleData(spec = spec, basePath = basePath)

  if (!is.data.frame(data)) {
    stop("Loaded example data is not a data.frame.", call. = FALSE)
  }

  dataContext = loadWMFMExampleContext(spec = spec, basePath = basePath)

  result = runModel(
    data = data,
    formula = stats::as.formula(spec$formula),
    modelType = spec$modelType,
    dataContext = dataContext,
    printOutput = printOutput,
    ...
  )

  invisible(result)
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
      stop("For file-based examples, the specification must contain a non-empty `data` entry.", call. = FALSE)
    }

    dataPath = file.path(basePath, spec$data)

    if (!file.exists(dataPath)) {
      stop("Data file not found: ", dataPath, call. = FALSE)
    }

    extension = tolower(tools::file_ext(dataPath))

    if (identical(extension, "csv")) {
      return(utils::read.csv(dataPath, stringsAsFactors = FALSE))
    }

    if (identical(extension, "txt")) {
      return(utils::read.table(dataPath, header = TRUE, stringsAsFactors = FALSE))
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
      stop("For package-based examples, the specification must contain a non-empty `dataPackage` entry.", call. = FALSE)
    }

    if (is.null(spec$dataObject) || !nzchar(spec$dataObject)) {
      stop("For package-based examples, the specification must contain a non-empty `dataObject` entry.", call. = FALSE)
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

    utils::data(
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

  if (!requireNamespace(spec$dataPackage, quietly = TRUE)) {
    return(NULL)
  }

  helpObject = utils::help(
    topic = spec$dataObject,
    package = spec$dataPackage
  )

  helpFile = tryCatch(
    utils:::.getHelpFile(helpObject),
    error = function(e) {
      NULL
    }
  )

  if (is.null(helpFile)) {
    return(NULL)
  }

  helpText = tryCatch(
    paste(capture.output(tools::Rd2txt(helpFile)), collapse = "\n"),
    error = function(e) {
      NULL
    }
  )

  helpText
}
