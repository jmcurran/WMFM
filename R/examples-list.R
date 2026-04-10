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
