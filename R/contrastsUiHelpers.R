#' Contrasts UI helper functions
#'
#' These functions create the UI pieces used by the contrasts module.
#'
#' @importFrom shiny tagList fluidRow column selectInput actionButton hr h5 helpText
#' @importFrom shiny conditionalPanel
#' @importFrom sortable bucket_list add_rank_list
#'
#' @export
contrastsPairsUi = function(levs) {

  tagList(
    fluidRow(
      column(
        4,
        selectInput("contrastLevel1", "Compare level:", choices = levs)
      ),
      column(
        4,
        selectInput("contrastLevel2", "Against level:", choices = levs)
      ),
      column(
        4,
        br(),
        actionButton("addContrastBtn", "Add contrast", class = "btn-primary")
      )
    ),
    hr(),
    fluidRow(
      column(
        8,
        selectInput(
          "contrastList",
          label = NULL,
          choices = character(0),
          multiple = TRUE,
          selectize = FALSE,
          size = 7
        )
      ),
      column(
        4,
        br(),
        actionButton("removeContrastBtn", "Remove selected", class = "btn-warning"),
        br(), br(),
        actionButton("clearContrastsBtn", "Clear all", class = "btn-secondary"),
        br(), br(),
        actionButton("computeContrastsBtn", "Compute contrasts", class = "btn-success")
      )
    )
  )
}

#' Drag-and-drop average contrast UI
#'
#' The user drags levels from the pool into left and right buckets. The contrast
#' is mean(left) - mean(right).
#'
#' @param levs Factor levels.
#' @export
contrastsAvgDragUi = function(levs) {

  tagList(
    helpText("Drag levels into the left and right boxes. The contrast is average(left) minus average(right)."),
    bucket_list(
      header = NULL,
      group_name = "avgBuckets",
      orientation = "horizontal",
      add_rank_list(text = "Levels", labels = levs, input_id = "avgPoolLevels"),
      add_rank_list(text = "Average of:", labels = character(0), input_id = "avgLeftLevels"),
      add_rank_list(text = "To average of:", labels = character(0), input_id = "avgRightLevels")
    ),
    hr(),
    actionButton("computeContrastsBtn", "Compute contrast", class = "btn-success")
  )
}

#' Custom contrast UI (text weights)
#'
#' Allows fractional weights like 1/2. Validation and compute behaviour lives in server.
#'
#' @param levs Factor levels.
#' @export
contrastsCustomUi = function(levs) {

  tagList(
    helpText("Enter coefficients (you may use fractions like 1/3). Coefficients must sum to 0."),
    # For simplicity: a textarea with one weight per line matching the level order.
    tags$div(
      tags$strong("Levels (in order):"),
      tags$pre(paste(levs, collapse = "\n"))
    ),
    tags$textarea(
      id = "customWeightsText",
      rows = max(6, length(levs)),
      style = "width: 100%; font-family: monospace;",
      placeholder = paste(rep("0", length(levs)), collapse = "\n")
    ),
    hr(),
    actionButton("computeContrastsBtn", "Compute contrast", class = "btn-success")
  )
}
