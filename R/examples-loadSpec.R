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

  spec = yaml::read_yaml(specPath)

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
