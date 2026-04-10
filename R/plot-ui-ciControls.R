#' Plot CI controls for the Plot tab
#'
#' UI controls for confidence intervals. The UI adapts to the plot type:
#' \itemize{
#'   \item \code{"factorOnly"}: show CI type (standard vs robust) and HC choice.
#'   \item \code{"continuous"}: show an optional "Show confidence intervals"
#'   checkbox; when enabled, show level, CI type, and HC choice.
#' }
#'
#' Designed to be placed in the Plot tab sidebar / controls area.
#'
#' @param mode Either \code{"factorOnly"} or \code{"continuous"}.
#' @param showCiInputId Input id for the "show confidence intervals" checkbox.
#' @param ciLevelInputId Input id for the confidence level slider.
#' @param ciTypeInputId Input id for CI type radio buttons.
#' @param hcTypeInputId Input id for HC type dropdown.
#'
#' @return A Shiny tag list.
#'
#' @importFrom shiny tagList checkboxInput sliderInput radioButtons selectInput
#' @importFrom shiny conditionalPanel tags icon
#'
#' @export
plotCiControlsUi = function(
    mode = c("factorOnly", "continuous"),
    showCiInputId  = "plotShowCi",
    ciLevelInputId = "plotCiLevel",
    ciTypeInputId  = "plotCiType",
    hcTypeInputId  = "plotHcType"
) {

  mode = match.arg(mode)

  hcHelp = paste(
    "HC0 is the basic heteroskedasticity/unequal variance-robust (sandwich) estimator.",
    "HC3 applies a leverage adjustment and is often more conservative,",
    "especially in smaller samples (typically wider intervals)."
  )

  ciHelp = paste(
    "Standard CIs use the model's usual variance assumptions.",
    "Robust (sandwich) CIs allow for heteroskedasticity/unequal variance;",
    "HC0/HC3 choose the robust variance estimator used for the interval."
  )

  levelHelp = paste(
    "The confidence level controls the width of the interval.",
    "For example, 95% intervals will be wider than 90% intervals."
  )

  ciTypeBlock = tagList(
    radioButtons(
      inputId = ciTypeInputId,
      label = tagList(
        "Confidence interval type ",
        tags$span(
          icon("circle-info"),
          title = ciHelp,
          style = "cursor: help;"
        )
      ),
      choices = c(
        "Standard (model-based)" = "standard",
        "Robust (sandwich)"      = "sandwich"
      ),
      selected = "standard",
      inline   = TRUE
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
        choices  = c("HC0", "HC3"),
        selected = "HC0",
        width    = "200px"
      )
    )
  )

  if (identical(mode, "factorOnly")) {
    return(tagList(ciTypeBlock))
  }

  # mode == "continuous"
  tagList(
    checkboxInput(
      inputId = showCiInputId,
      label   = "Show confidence intervals",
      value   = FALSE
    ),
    conditionalPanel(
      condition = sprintf("input.%s", showCiInputId),
      sliderInput(
        inputId = ciLevelInputId,
        label = tagList(
          "Confidence level ",
          tags$span(
            icon("circle-info"),
            title = levelHelp,
            style = "cursor: help;"
          )
        ),
        min   = 0.80,
        max   = 0.99,
        value = 0.95,
        step  = 0.01
      ),
      ciTypeBlock
    )
  )
}
