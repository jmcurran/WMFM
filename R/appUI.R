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

    uiOutput("main_tabs_ui")

  )
}
