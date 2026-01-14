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
#' @importFrom shiny h4 h5 uiOutput tabsetPanel tabPanel
#' @importFrom shiny radioButtons textInput verbatimTextOutput
#' @importFrom shiny br actionButton plotOutput helpText
#' @importFrom shiny conditionalPanel selectInput div checkboxInput textOutput
#' @importFrom bslib accordion accordion_panel bs_theme
appUI = function() {
  fluidPage(
    theme = bs_theme(),  # enable bslib components (Bootstrap 5)
    withMathJax(),
    titlePanel("What's My Fitted Model?"),

    tags$style(HTML("
      .bucket-list .rank-list {
        max-height: 8em;
        overflow-y: auto;
      }
      body { font-size: 90%; }
      .shiny-input-container { font-size: 90%; }
      .nav-tabs > li > a { font-size: 90%; }
      pre, code { font-size: 90%; }
      /* Derived-variable row: align nicely */
      .wmfmDerivedVar .shiny-input-container {
        margin-bottom: 0 !important;
      }

      .wmfmDerivedVarRow {
        display: flex;
        align-items: center;
        gap: 12px;
      }

      /* Keep the input to a sensible width so the button sits just after it */
      .wmfmDerivedVarRow .wmfmDerivedVarInput {
        flex: 0 1 620px;   /* change 620px if you want longer/shorter */
      }
    ")),

    tabsetPanel(
      id = "main_tabs",

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
        ),

        tags$div(
          style = "font-size: 0.8em; color: #666; margin-top: 20px;",
          paste("WMFM version", as.character(packageVersion("WMFM")))
        )
      ),


      tabPanel(
        "Model",
        tagList(
          h4("Assign variables"),
          uiOutput("var_buckets"),
          uiOutput("interaction_ui"),

          hr(),

          div(
            class = "wmfmDerivedVar",
            tags$h4("Create a derived variable"),

            div(
              class = "wmfmDerivedVarRow",
              div(
                class = "wmfmDerivedVarInput",
                textInput(
                  inputId = "derivedVarText",
                  label = NULL,
                  placeholder = "e.g. t = 1:nrow(data)    or    month = factor(rep(1:12, 12))",
                  value = ""
                )
              ),
              actionButton(
                "addDerivedVarBtn",
                "Add variable",
                class = "btn btn-success"
              )
            ),

            div(style = "margin-top: 6px;"),
            textOutput("derivedVarMsg")
          ),

          hr(),

          h4("Response, model type, and fitting"),
          fluidRow(
            column(
              width = 4,
              h5("Response"),
              uiOutput("response_picker")
            ),

            column(
              width = 4,
              h5("Type"),
              radioButtons(
                "model_type",
                label = NULL,
                choices = c(
                  "Linear regression" = "lm",
                  "Logistic regression (binomial, logit)" = "logistic",
                  "Poisson regression (log link)" = "poisson"
                ),
                selected = "lm"
              )
            ),

            column(
              width = 4,
              h5(""),
              div(
                style = "margin-top: 6px;",
                actionButton(
                  "fit_btn",
                  "Fit model",
                  class = "btn-primary btn-sm"
                ),
                tags$br(), tags$br(),
                actionButton(
                  "reset_btn",
                  "Reset model",
                  class = "btn-secondary btn-sm"
                )
              )
            )
          ),

          hr(),

          h4("Model formula"),
          checkboxInput(
            "expert_mode",
            "Use compact (expert) formula notation where possible",
            value = FALSE
          ),
          textInput("formula_text", label = NULL, value = "", width = "100%"),
          verbatimTextOutput("formula_status")
        )
      ),

      tabPanel(
        "Fitted Model",
        h4("Model equation"),
        uiOutput("model_formula"),

        hr(),

        uiOutput("model_equations_header"),
        uiOutput("fitted_means"),
        uiOutput("model_equations"),

        hr(),

        h4("Model outputs"),
        accordion(
          id = "model_outputs",
          multiple = TRUE,
          open = NULL,
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

      tabPanel("Contrasts", uiOutput("tab_contrasts")),
      tabPanel("Plot", uiOutput("tab_plot"))
    )

  )
}
