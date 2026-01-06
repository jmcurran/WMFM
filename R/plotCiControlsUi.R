#' Plot CI controls for the Plot tab
#'
#' UI controls to select the confidence interval type (standard vs robust) and,
#' for robust intervals, the sandwich HC estimator (HC0 vs HC3).
#'
#' Includes small hover tooltips to explain the choices.
#'
#' Designed to be placed in the Plot tab sidebar / controls area.
#'
#' @param ciTypeInputId Input id for CI type radio buttons.
#' @param hcTypeInputId Input id for HC type dropdown.
#'
#' @return A Shiny tag list.
#'
#' @importFrom shiny tagList radioButtons selectInput conditionalPanel tags icon
#'
#' @export
plotCiControlsUi = function(
    ciTypeInputId = "plotCiType",
    hcTypeInputId = "plotHcType"
) {

  hcHelp = paste(
    "HC0 is the basic heteroskedasticity/unequal variance-robust (sandwich) estimator.",
    "HC3 applies a leverage adjustment and is often more conservative,",
    "especially in smaller samples (typically wider intervals)."
  )

  ciHelp = paste(
    "Standard CIs use the model's usual variance assumptions.",
    "Robust (sandwich) CIs allow for heteroskedasticity/unequal variance; HC0/HC3 choose the",
    "robust variance estimator used for the interval."
  )

  tagList(
    radioButtons(
      inputId = ciTypeInputId,
      label = tagList(
        "Confidence intervals for fitted means ",
        tags$span(
          icon("circle-info"),
          title = ciHelp,
          style = "cursor: help;"
        )
      ),
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
        label = tagList(
          "Robust CI type (sandwich) ",
          tags$span(
            icon("circle-info"),
            title = hcHelp,
            style = "cursor: help;"
          )
        ),
        choices = c("HC0", "HC3"),
        selected = "HC0",
        width = "200px"
      )
    )
  )
}
