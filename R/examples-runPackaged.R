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
#' @importFrom utils read.csv
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
