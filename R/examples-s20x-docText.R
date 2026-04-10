#' Get s20x dataset documentation as plain text
#'
#' @param dataset Character string, name of a data set in the s20x package.
#'
#' @return A single character string with the Rd help text, or NULL if not found.
#' @keywords internal
#' @importFrom tools Rd2txt
#' @importFrom utils capture.output
getS20xDocText = function(dataset) {
  rd = getInstalledPackageRd(
    topic = dataset,
    package = "s20x"
  )

  if (is.null(rd)) {
    return(NULL)
  }

  txt = capture.output(Rd2txt(rd, out = ""))

  paste(txt, collapse = "\n")
}
