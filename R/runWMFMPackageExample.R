#' Run a packaged WMFM example
#'
#' Loads a stored example from the package's `inst/extdata/examples`
#' directory, reads its specification, data, and optional context, and then
#' runs `runWMFMModelDebug()` using those inputs.
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
#' @param printOutput Logical. Passed to `runWMFMModelDebug()`. If `TRUE`,
#'   prints the model summary, fitted equations, and explanation.
#' @param ... Additional arguments passed on to `runWMFMModelDebug()`.
#'
#' @return Invisibly returns the result of `runWMFMModelDebug()`.
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

  result = runWMFMModelDebug(
    data = data,
    formula = stats::as.formula(spec$formula),
    modelType = spec$modelType,
    dataContext = dataContext,
    printOutput = printOutput,
    ...
  )

  invisible(result)
}
