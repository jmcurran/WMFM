#' Get s20x dataset documentation as plain text
#'
#' @param dataset Character string, name of a data set in the s20x package.
#'
#' @return A single character string with the Rd help text, or NULL if not found.
#' @keywords internal
getS20xDocText = function(dataset) {
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

  txt = utils::capture.output(tools::Rd2txt(rd, out = ""))
  paste(txt, collapse = "\n")
}
