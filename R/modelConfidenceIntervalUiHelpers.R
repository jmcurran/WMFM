#' Build choices for the confidence-interval row selector
#'
#' @param ciData Output from \code{buildModelConfidenceIntervalData()}.
#'
#' @return A named character vector suitable for \code{selectInput()}.
#' @keywords internal
buildModelConfidenceIntervalRowChoices = function(ciData) {

  if (is.null(ciData$details) || length(ciData$details) == 0) {
    return(c("Choose a row..." = ""))
  }

  labels = vapply(ciData$details, function(x) x$label, character(1))
  values = c("", labels)
  stats::setNames(values, c("Choose a row...", labels))
}

#' Find a selected confidence-interval detail record
#'
#' @param ciData Output from \code{buildModelConfidenceIntervalData()}.
#' @param selectedLabel Selected row label from the UI.
#'
#' @return A single detail record or \code{NULL}.
#' @keywords internal
findModelConfidenceIntervalDetail = function(ciData, selectedLabel) {

  if (is.null(selectedLabel) || !nzchar(selectedLabel)) {
    return(NULL)
  }

  if (is.null(ciData$details) || length(ciData$details) == 0) {
    return(NULL)
  }

  matches = vapply(ciData$details, function(x) identical(x$label, selectedLabel), logical(1))

  if (!any(matches)) {
    return(NULL)
  }

  ciData$details[[which(matches)[1]]]
}

#' Render a calm labelled confidence-interval explanation panel
#'
#' @param detail A single detail record.
#'
#' @return A Shiny tag list.
#' @keywords internal
#'
#' @importFrom shiny tagList tags
renderModelConfidenceIntervalDetailUi = function(detail) {

  if (is.null(detail)) {
    return(NULL)
  }

  makeBlock = function(label, value) {
    shiny::tags$div(
      style = "margin-bottom: 10px;",
      shiny::tags$div(
        style = "font-weight: 600; margin-bottom: 2px;",
        label
      ),
      shiny::tags$div(value)
    )
  }

  shiny::tags$div(
    style = paste(
      "border: 1px solid #d9d9d9;",
      "border-radius: 6px;",
      "padding: 12px;",
      "background-color: #fafafa;",
      sep = " "
    ),
    makeBlock("Quantity", detail$quantity %||% ""),
    makeBlock("Settings", detail$settings %||% ""),
    makeBlock("Built from coefficients", detail$builtFrom %||% ""),
    makeBlock("Variance formula", detail$varianceFormula %||% ""),
    makeBlock("Scale note", detail$scaleNote %||% "")
  )
}
