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
