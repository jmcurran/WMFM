#' Application user interface
#'
#' Constructs the Shiny user interface for the Model Builder app.
#'
#' The interface is organised into the main teaching and analysis tabs:
#' \itemize{
#'   \item \strong{Load Data}: upload a data set or choose an example from
#'         the \pkg{s20x} package.
#'   \item \strong{Model}: assign variables to roles, choose the model type,
#'         specify the model formula, and fit/reset the model.
#'   \item \strong{Fitted Model}: inspect the symbolic model equation, fitted
#'         equations from the language model, and the main regression outputs.
#'   \item \strong{Model Explanation}: view the plain-language explanation,
#'         an optional tutor-style expansion, and the teaching accordions that
#'         explain how the explanation was constructed.
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
#' @importFrom shiny conditionalPanel selectInput div checkboxInput textOutput passwordInput
#' @importFrom shiny sidebarLayout sidebarPanel mainPanel tableOutput
#' @importFrom bslib accordion accordion_panel bs_theme
appUI = function() {
  fluidPage(
    theme = bs_theme(),
    withMathJax(),
    titlePanel("What's My Fitted Model?"),

    tags$style(HTML("\n      .bucket-list .rank-list {\n        max-height: 8em;\n        overflow-y: auto;\n      }\n      body { font-size: 90%; }\n      .shiny-input-container { font-size: 90%; }\n      .nav-tabs > li > a { font-size: 90%; }\n      pre, code { font-size: 90%; }\n\n      h4 {\n        margin-top: 12px;\n        margin-bottom: 8px;\n      }\n\n      h5 {\n        margin-top: 6px;\n        margin-bottom: 4px;\n      }\n\n      hr {\n        margin: 8px 0;\n      }\n\n      .hr-tight {\n        margin: 6px 0;\n      }\n\n      .form-group {\n        margin-bottom: 8px;\n      }\n\n      .radio {\n        margin-top: 3px;\n        margin-bottom: 3px;\n      }\n\n      .shiny-html-output,\n      .shiny-text-output {\n        margin-bottom: 6px;\n      }\n\n      .wmfm-ci-section-label {\n        font-weight: 600;\n        margin-top: 10px;\n        margin-bottom: 4px;\n      }\n\n      .wmfm-ci-drilldown-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #fcfcfc;\n        margin-top: 10px;\n        margin-bottom: 10px;\n      }\n\n      .wmfm-ci-secondary-note {\n        color: #666;\n        margin-bottom: 8px;\n      }\n\n      .wmfm-ci-collapsible-block {\n        margin-top: 10px;\n      }\n\n      .wmfm-explanation-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #fcfcfc;\n        margin-top: 8px;\n        white-space: normal;\n      }\n\n      .wmfm-explanation-box p {\n        margin: 0 0 0.8em 0;\n      }\n\n      .wmfm-explanation-box p:last-child {\n        margin-bottom: 0;\n      }\n\n      .wmfm-explanation-helper-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #f8f9fb;\n        margin-top: 10px;\n        margin-bottom: 10px;\n      }\n\n      .wmfm-explanation-helper-note {\n        color: #666;\n        margin-bottom: 8px;\n      }\n    ")),

    tabsetPanel(
      id = "main_tabs",

      tabPanel(
        "Load Data",
        h4("Load a data set"),

        radioButtons(
          "data_source",
          label = "Data source:",
          choices = c(
            "Upload file" = "upload",
            "Data from installed package" = "package"
          ),
          selected = "package"
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
          condition = "input.data_source == 'package'",
          selectInput(
            "data_package",
            "Choose a package:",
            choices = character(0)
          ),
          selectInput(
            "package_dataset",
            "Choose a dataset:",
            choices = character(0)
          ),
          helpText(
            "Packages listed here are installed packages that appear to contain datasets."
          ),
          div(
            style = "font-size: 0.85em; color: #666;",
            textOutput("packageScanStatus")
          )
        ),

        tags$hr(class = "hr-tight"),
        h4("Load a built-in example"),
        selectInput(
          "exampleName",
          "Choose an example:",
          choices = character(0)
        ),
        actionButton(
          "loadExampleBtn",
          "Load example",
          class = "btn btn-secondary"
        ),
        div(
          style = "margin-top: 10px;",
          helpText(
            "Examples load a known working dataset together with its research question and model settings."
          )
        ),
        div(
          style = "font-size: 0.85em; color: #666;",
          textOutput("exampleLoadStatus")
        ),

        hr(),

        tags$div(
          style = "font-size: 0.8em; color: #666; margin-top: 20px;",
          paste("WMFM version", as.character(packageVersion("WMFM")))
        )
      ),

      tabPanel(
        "Model",
        tagList(
          h5("Select response variable"),
          fluidRow(
            column(
              width = 8,
              uiOutput("response_picker")
            ),
            column(
              width = 4,
              div(
                style = "margin-top: 18px;",
                uiOutput(outputId = "modelHelpBtnUi")
              )
            )
          ),
          helpText(
            "After loading data, go to the Model tab to assign variables and specify the regression model."
          ),
          uiOutput("response_explain"),
          uiOutput("userDatasetContextUi"),

          tags$hr(class = "hr-tight"),

          h5("Research question"),
          textInput(
            "researchQuestion",
            label = "Research question",
            value = "",
            width = "100%"
          ),
          helpText(
            "Briefly state the question you want the fitted model to help answer. ",
            "WMFM uses this to frame the explanation from the start, so it should feel like the reason for the analysis rather than an optional extra."
          ),

          tags$hr(class = "hr-tight"),

          h5("Assign explanatory/predictor variables"),
          uiOutput("var_buckets"),
          div(
            actionButton(
              "addDerivedVarBtn",
              "Add derived variable",
              class = "btn btn-success"
            )
          ),

          uiOutput("interaction_ui"),

          tags$hr(class = "hr-tight"),

          h5("Model type and model fitting"),
          fluidRow(
            column(
              width = 8,
              radioButtons(
                "model_type",
                label = "Model type:",
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

          tags$hr(class = "hr-tight"),

          h5("Model formula"),
          textInput("formula_text", label = NULL, value = "", width = "100%"),
          verbatimTextOutput("formula_status"),
          checkboxInput(
            "expert_mode",
            "Use compact (expert) formula notation where possible",
            value = FALSE
          )
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
            "Model outputs",
            value = "model_output_tabs",
            tabsetPanel(
              tabPanel(
                "Summary",
                verbatimTextOutput("model_output")
              ),
              tabPanel(
                "ANOVA",
                verbatimTextOutput("model_anova")
              ),
              tabPanel(
                "Confidence intervals",
                uiOutput("modelConfintControlsUi"),
                uiOutput("modelConfintNoteUi"),
                uiOutput("modelConfintTableUi"),
                tags$div(
                  class = "wmfm-ci-drilldown-box",
                  tags$div(
                    class = "wmfm-ci-section-label",
                    "Explain one interval"
                  ),
                  tags$div(
                    class = "wmfm-ci-secondary-note",
                    "Choose a single row when you want to unpack how that interval was constructed."
                  ),
                  uiOutput("modelConfintSelectorUi"),
                  uiOutput("modelConfintSelectedRowUi")
                ),
                accordion(
                  id = "model_confint_extras",
                  multiple = TRUE,
                  open = FALSE,
                  accordion_panel(
                    "Teaching note",
                    value = "model_confint_teaching_note",
                    uiOutput("modelConfintTeachingNoteUi")
                  ),
                  accordion_panel(
                    "Variance-covariance matrix",
                    value = "model_confint_vcov",
                    uiOutput("modelConfintVcovUi")
                  )
                )
              )
            )
          )
        )
      ),

      tabPanel(
        "Model Explanation",
        h4("Model explanation"),
        helpText(
          "Start with the main explanation, then use the sections below to see how to read it, what information the app relied on, and where each sentence came from."
        ),
        uiOutput("model_explanation")
      ),

      tabPanel(
        "Contrasts",
        uiOutput("contrasts_content_ui")
      ),

      tabPanel(
        "Variable Explorer",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "veVar",
              label = "Select a variable",
              choices = character(0)
            )
          ),
          mainPanel(
            uiOutput("veSummaryUi"),
            plotOutput("vePlot", height = "320px")
          )
        )
      ),

      tabPanel(
        "Plot",
        h4("Data and fitted model"),
        uiOutput("plot_ci_controls_ui"),
        plotOutput("model_plot"),
        helpText(
          "The plot shows the observed data and the fitted model ",
          "against one numeric predictor (x-axis), optionally separated by a factor."
        )
      ),

      tabPanel(
        "Settings",
        h4("Chat provider"),
        helpText(
          "Choose which language model backend WMFM should use for equations and explanations."
        ),
        selectInput(
          inputId = "chat_provider",
          label = "Provider",
          choices = c(
            "Ollama" = "ollama",
            "Claude" = "claude"
          ),
          selected = "ollama"
        ),
        conditionalPanel(
          condition = "input.chat_provider == 'ollama'",
          selectInput(
            inputId = "ollama_model",
            label = "Ollama model",
            choices = c("gpt-oss"),
            selected = "gpt-oss"
          ),
          actionButton(
            inputId = "refreshOllamaModelsBtn",
            label = "Refresh available models",
            class = "btn btn-secondary btn-sm"
          ),
          helpText(
            "WMFM will query the configured Ollama server for available models. The default is gpt-oss when it is available."
          )
        ),
        passwordInput(
          inputId = "providerSwitchPassword",
          label = "Password required to switch to Claude",
          placeholder = "Enter password only when switching to Claude"
        ),
        actionButton(
          inputId = "applyChatProviderBtn",
          label = "Apply provider",
          class = "btn-primary btn-sm"
        ),
        tags$br(), tags$br(),
        textOutput("chatProviderStatus"),
        helpText(
          "Ollama can be selected directly, with a specific Ollama model chosen from the server. Switching to Claude requires the provider password and a configured ANTHROPIC_API_KEY on the machine running the app."
        )
      )

    )

  )
}
