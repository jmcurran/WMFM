#' Get s20x dataset documentation as HTML
#'
#' @param dataset Character string, name of a data set in the s20x package.
#'
#' @return A single character string containing HTML for the help page, or NULL if not found.
#' @keywords internal
#' @importFrom tools Rd2HTML
getS20xDocHtml = function(dataset) {
  rd = getInstalledPackageRd(
    topic = dataset,
    package = "s20x"
  )

  if (is.null(rd)) {
    return(NULL)
  }

  tmpHtml = tempfile(fileext = ".html")

  ok = tryCatch({
    Rd2HTML(rd, out = tmpHtml)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!ok || !file.exists(tmpHtml)) {
    return(NULL)
  }

  paste(readLines(tmpHtml, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
