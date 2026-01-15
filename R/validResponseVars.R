#' List valid response variables for a model type
#'
#' Returns the names of variables in a data frame that are supported
#' as response variables for the specified model type.
#'
#' @param data A data frame.
#' @param modelType Model type passed to \code{validateResponseVar()}.
#'
#' @return A character vector of variable names.
#'
#' @keywords internal
validResponseVars = function(data, modelType) {

  if (!is.data.frame(data)) {
    return(character(0))
  }

  vars = names(data)

  ok = vapply(
    vars,
    function(v) validateResponseVar(data, v, modelType)$ok,
    logical(1)
  )

  vars[ok]
}
