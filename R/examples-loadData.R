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
