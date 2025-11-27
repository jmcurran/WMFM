#' Application user interface
#'
#' Constructs the Shiny user interface for the Model Builder app.
#'
#' The interface is organised into four tabs:
#' \itemize{
#'   \item \strong{Load Data}: upload a data set or choose an example from
#'         the \pkg{s20x} package.
#'   \item \strong{Model}: assign variables to roles, choose the model type,
#'         specify the model formula, and fit/reset the model.
#'   \item \strong{Fitted Model}: inspect the symbolic model equation, fitted
#'         equations from the language model, the regression table, and a
#'         plain-language explanation.
#'   \item \strong{Plot}: view the observed data with the fitted model
#'         overlaid when there is a single numeric predictor.
#' }
#'
#' @return A Shiny UI definition created with \code{fluidPage()}.
#'
#' @keywords internal
#'
#' @importFrom shiny fluidPage withMathJax titlePanel
#' @importFrom shiny tags HTML fluidRow column fileInput hr
#' @importFrom shiny h4 uiOutput tabsetPanel tabPanel
#' @importFrom shiny radioButtons textInput verbatimTextOutput
#' @importFrom shiny br actionButton plotOutput helpText
#' @importFrom shiny conditionalPanel selectInput
#' @importFrom bslib accordion accordion_panel
appUI = function() {
  fluidPage(
    withMathJax(),
    titlePanel("Model Builder"),

    tags$style(HTML("
      .bucket-list .rank-list {
        max-height: 8em;
        overflow-y: auto;
      }
    ")),

    tabsetPanel(
      id = "main_tabs",

      # ---- Tab 1: Load Data ----
      tabPanel(
        "Load Data",
        h4("Load a data set"),

        radioButtons(
          "data_source",
          label = "Data source:",
          choices = c(
            "Upload file"               = "upload",
            "Example from s20x package" = "s20x"
          ),
          selected = "upload"
        ),

        # Upload UI
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          fluidRow(
            column(
              12,
              fileInput(
                "file",
                "Choose CSV, TXT, or RDA",
                accept = c(".csv", ".txt", ".rda", ".RData")
              )
            )
          ),
          helpText(
            "Upload a CSV, TXT, or RDA file containing a single data frame."
          )
        ),

        # s20x dataset UI
        conditionalPanel(
          condition = "input.data_source == 's20x'",
          selectInput(
            "s20x_dataset",
            "Choose an s20x example data set:",
            choices = character(0)
          ),
          helpText(
            "These data sets are shipped with the s20x package and are useful ",
            "for examples and teaching."
          )
        ),

        hr(),
        helpText(
          "After loading data, go to the Model tab to assign variables ",
          "and specify the regression model."
        )
      ),

      # ---- Tab 2: Model specification / variable assignment ----
      tabPanel(
        "Model",
        h4("Assign variables"),
        uiOutput("var_buckets"),

        hr(),

        # Model type + Fit/Reset buttons side by side
        fluidRow(
          column(
            width = 8,
            h4("Model type"),
            radioButtons(
              "model_type",
              label = NULL,
              choices = c(
                "Linear regression"                     = "lm",
                "Logistic regression (binomial, logit)" = "logistic",
                "Poisson regression (log link)"         = "poisson"
              ),
              selected = "lm"
            )
          ),
          column(
            width = 4,
            br(),  # space above buttons
            actionButton(
              "fit_btn",
              "Fit model",
              width = "100%"
            ),
            br(),
            actionButton(
              "reset_btn",
              "Reset model",
              width = "100%"
            )
          )
        ),

        hr(),

        h4("Response"),
        uiOutput("response_picker"),

        h4("Model formula"),
        textInput("formula_text", label = NULL, value = "", width = "100%"),
        verbatimTextOutput("formula_status")
      ),

      # ---- Tab 3: Fitted model outputs ----
      tabPanel(
        "Fitted Model",
        h4("Model equation"),
        uiOutput("model_formula"),

        hr(),

        h4("Fitted equations"),
        uiOutput("model_equations"),

        hr(),

        h4("Model outputs"),
        accordion(
          id = "model_outputs",
          accordion_panel(
            "Regression table",
            value = "reg_table",
            verbatimTextOutput("model_output")
          ),
          accordion_panel(
            "Model explanation",
            value = "model_expl",
            uiOutput("model_explanation")
          )
        )
      ),

      # ---- Tab 4: Plot ----
      tabPanel(
        "Plot",
        h4("Data and fitted model"),
        plotOutput("model_plot"),
        helpText(
          "The plot shows the observed data and the fitted model ",
          "against one numeric predictor (x-axis), optionally separated by a factor."
        )
      )
    )
  )
}
