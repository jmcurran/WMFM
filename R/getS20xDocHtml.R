#' Get s20x dataset documentation as HTML
#'
#' @param dataset Character string, name of a data set in the s20x package.
#'
#' @return A single character string containing HTML for the help page, or NULL if not found.
#' @keywords internal
getS20xDocHtml = function(dataset) {
  if (!requireNamespace("s20x", quietly = TRUE)) {
    return(NULL)
  }

  hf = utils::help(dataset, package = "s20x")
  if (length(hf) == 0L) {
    return(NULL)
  }

  # Get Rd object from help file (using internal helper via getFromNamespace)
  getHelpFile = getFromNamespace(".getHelpFile", "utils")
  rd = getHelpFile(hf)

  # Convert Rd to HTML file then read back
  tmpHtml = tempfile(fileext = ".html")

  ok = tryCatch({
    tools::Rd2HTML(rd, out = tmpHtml)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!ok || !file.exists(tmpHtml)) {
    return(NULL)
  }

  paste(readLines(tmpHtml, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
