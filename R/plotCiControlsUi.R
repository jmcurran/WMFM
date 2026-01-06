#' Plot CI controls for the Plot tab
#'
#' UI controls to select the confidence interval type (standard vs robust) and,
#' for robust intervals, the sandwich HC estimator (HC0 vs HC3).
#'
#' Designed to be placed in the Plot tab sidebar / controls area.
#'
#' @param ciTypeInputId Input id for CI type radio buttons.
#' @param hcTypeInputId Input id for HC type dropdown.
#'
#' @return A Shiny tag list.
#'
#' @importFrom shiny tagList radioButtons selectInput conditionalPanel
#'
#' @export
plotCiControlsUi = function(
  ciTypeInputId = "plotCiType",
  hcTypeInputId = "plotHcType"
) {
  tagList(
    radioButtons(
      inputId = ciTypeInputId,
      label = "Confidence intervals for fitted means",
      choices = c(
        "Standard (model-based)" = "standard",
        "Robust (sandwich)" = "sandwich"
      ),
      selected = "standard",
      inline = TRUE
    ),
    conditionalPanel(
      condition = sprintf("input.%s == 'sandwich'", ciTypeInputId),
      selectInput(
        inputId = hcTypeInputId,
        label = "Robust CI type (sandwich)",
        choices = c("HC0", "HC3"),
        selected = "HC0",
        width = "200px"
      )
    )
  )
}
