#' Application user interface
#'
#' Constructs the Shiny user interface for the Model Builder app.
#'
#' The interface is organised into the main teaching and analysis tabs:
#' \itemize{
#'   \item \strong{Load}: upload a data set or choose an example from
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
#' @importFrom shiny textInput textAreaInput verbatimTextOutput
#' @importFrom shiny br actionButton plotOutput helpText
#' @importFrom shiny conditionalPanel selectInput div checkboxInput textOutput passwordInput
#' @importFrom shiny sidebarLayout sidebarPanel mainPanel tableOutput
#' @importFrom bslib accordion accordion_panel bs_theme
appUI = function() {
  fluidPage(
    theme = bs_theme(),
    withMathJax(),
    titlePanel("What's My Fitted Model?"),

    tags$style(HTML("\n      .bucket-list .rank-list {\n        max-height: 8em;\n        overflow-y: auto;\n      }\n      html, body {\n        min-height: 100%;\n        overflow-y: auto;\n      }\n      body {\n        font-size: 90%;\n      }\n      .container-fluid {\n        padding-bottom: 18px;\n      }\n      .shiny-input-container { font-size: 90%; }\n      .nav-tabs > li > a { font-size: 90%; }\n      pre, code { font-size: 90%; }\n\n      h4 {\n        margin-top: 12px;\n        margin-bottom: 8px;\n      }\n\n      h5 {\n        margin-top: 6px;\n        margin-bottom: 4px;\n      }\n\n      hr {\n        margin: 8px 0;\n      }\n\n      .hr-tight {\n        margin: 6px 0;\n      }\n\n      .form-group {\n        margin-bottom: 8px;\n      }\n\n      .radio {\n        margin-top: 3px;\n        margin-bottom: 3px;\n      }\n\n      .shiny-html-output,\n      .shiny-text-output {\n        margin-bottom: 6px;\n      }\n\n      .wmfm-ci-section-label {\n        font-weight: 600;\n        margin-top: 10px;\n        margin-bottom: 4px;\n      }\n\n      .wmfm-ci-drilldown-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #fcfcfc;\n        margin-top: 10px;\n        margin-bottom: 10px;\n      }\n\n      .wmfm-ci-secondary-note {\n        color: #666;\n        margin-bottom: 8px;\n      }\n\n      .wmfm-ci-collapsible-block {\n        margin-top: 10px;\n      }\n\n      .wmfm-explanation-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #fcfcfc;\n        margin-top: 8px;\n        white-space: normal;\n      }\n\n      .wmfm-explanation-box p {\n        margin: 0 0 0.8em 0;\n      }\n\n      .wmfm-explanation-box p:last-child {\n        margin-bottom: 0;\n      }\n\n      .wmfm-explanation-helper-box {\n        border: 1px solid #d9d9d9;\n        border-radius: 6px;\n        padding: 12px;\n        background-color: #f8f9fb;\n        margin-top: 10px;\n        margin-bottom: 10px;\n      }\n\n      .wmfm-explanation-helper-note {\n        color: #666;\n        margin-bottom: 8px;\n      }\n\n      .wmfm-model-tab h5 {\n        margin-top: 6px;\n        margin-bottom: 4px;\n      }\n\n      .wmfm-model-tab {\n        padding-bottom: 16px;\n      }\n\n      .wmfm-model-tab .help-block {\n        margin-bottom: 6px;\n      }\n\n      .wmfm-model-tab .form-group {\n        margin-bottom: 8px;\n      }\n\n      .wmfm-model-tab .hr-tight {\n        margin: 6px 0;\n      }\n\n      .wmfm-model-tab #formula_text {\n        margin-bottom: 4px;\n      }\n\n      .wmfm-model-tab #formula_status {\n        margin-top: 4px;\n        margin-bottom: 0;\n        min-height: 1.4em;\n      }\n\n      .wmfm-formula-status {\n        display: inline-block;\n        padding: 2px 8px;\n        border-radius: 12px;\n        font-size: 0.9em;\n        font-weight: 600;\n      }\n\n      .wmfm-formula-status-ok {\n        background-color: #e8f5e9;\n        color: #1b5e20;\n        border: 1px solid #c8e6c9;\n      }\n\n      .wmfm-formula-status-error {\n        background-color: #ffebee;\n        color: #b71c1c;\n        border: 1px solid #ffcdd2;\n      }\n\n      .wmfm-model-tab .checkbox {\n        margin-top: 6px;\n        margin-bottom: 6px;\n      }\n\n      .wmfm-model-fit-buttons {\n        display: flex;\n        flex-direction: column;\n        align-items: flex-start;\n        gap: 8px;\n      }\n\n      .wmfm-model-fit-buttons .btn {\n        margin-bottom: 0;\n      }\n\n      .tab-content {\n        overflow: visible;\n      }\n    ")),

    tabsetPanel(
      id = "main_tabs",

      tabPanel(
        "Load",
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
            choices = c("Loading packages..." = "")
          ),
          selectInput(
            "package_dataset",
            "Choose a dataset:",
            choices = c("Loading datasets..." = "")
          ),
          helpText(
            "Packages listed here are installed packages that appear to contain datasets."
          ),
          div(
            style = "font-size: 0.85em; color: #666;",
            textOutput("packageScanStatus")
          ),
          div(
            style = "font-size: 0.85em; color: #666;",
            textOutput("packageDatasetStatus")
          )
        ),
        tags$hr(class = "hr-tight"),
        h4("Load a built-in example"),
        selectInput(
          "exampleName",
          "Choose an example:",
          choices = c("Loading examples..." = "")
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

        div(
          style = "margin-top: 10px;",
          helpText(
            "After loading data, or an example, go to the Model tab to assign variables and specify, or change, the regression model."
          )
        ),

        hr(),

        tags$div(
          style = "font-size: 0.8em; color: #666; margin-top: 20px;",
          paste("WMFM version", utils::packageDescription("WMFM")$Version)
        )
      ),

      tabPanel(
        "Model",
        div(
          class = "wmfm-model-tab",
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
                style = "margin-top: 8px;",
                uiOutput(outputId = "modelHelpBtnUi")
              )
            )
          ),
          uiOutput("response_explain"),

          tags$hr(class = "hr-tight"),

          h5(
            "Research question ",
            tags$span(
              icon("circle-info"),
              title = paste(
                "Briefly state the question you want the fitted model to help answer.",
                "WMFM uses this to frame the explanation from the start,",
                "so it should feel like the reason for the analysis rather than an optional extra."
              ),
              style = "cursor: help;"
            )
          ),
          textInput(
            "researchQuestion",
            label = NULL,
            placeholder = "For example, how does the expected response change as the predictor changes?",
            value = "",
            width = "100%"
          ),
          accordion(
            id = "model_question_accordion",
            multiple = TRUE,
            open = FALSE,
            accordion_panel(
              title = "Optional follow-up question",
              helpText(
                "Ask an optional follow-up question about the fitted model, predictions, or interpretation."
              ),
              textAreaInput(
                inputId = "modelFollowupQuestion",
                label = NULL,
                value = "",
                width = "100%",
                rows = 3,
                placeholder = paste(
                  "Predict the response for x = 10\n",
                  "Explain this for a 10-unit increase"
                )
              )
            )
          ),

          tags$hr(class = "hr-tight"),

          h5("Assign explanatory/predictor variables"),
          uiOutput("var_buckets"),
          uiOutput("adjustment_variables_ui"),
          fluidRow(
            class = "wmfm-optional-controls-row",
            column(
              width = 4,
              div(
                actionButton(
                  "addDerivedVarBtn",
                  "Add derived variable",
                  class = "btn btn-success"
                )
              )
            ),
            column(
              width = 8,
              uiOutput("interaction_ui")
            )
          ),

          tags$hr(class = "hr-tight"),

          h5("Model type and model fitting"),
          fluidRow(
            column(
              width = 8,
              selectInput(
                "model_type",
                label = "Model type:",
                choices = c(
                  "Linear regression" = "lm",
                  "Logistic regression (binomial, logit)" = "logistic",
                  "Poisson regression (log link)" = "poisson"
                ),
                selected = "lm",
                width = "100%"
              )
            ),

            column(
              width = 4,
              h5(""),
              div(
                class = "wmfm-model-fit-buttons",
                style = "margin-top: 0;",
                actionButton(
                  "fit_btn",
                  "Fit model",
                  class = "btn-primary btn-sm"
                ),
                actionButton(
                  "reset_btn",
                  "Reset model",
                  class = "btn-secondary btn-sm"
                )
              )
            )
          ),

          tags$hr(class = "hr-tight"),

          fluidRow(
            column(
              width = 8,
              h5("Model formula"),
              textInput("formula_text", label = NULL, value = "", width = "100%")
            ),
            column(
              width = 4,
              h5(""),
              checkboxInput(
                "expert_mode",
                "Use compact interaction formula",
                value = FALSE
              ),
              uiOutput("formula_status")
            )
          )
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
        tabsetPanel(
          id = "model_output_tabs",
          tabPanel(
            "Summary",
            uiOutput("modelSummaryControlsUi"),
            uiOutput("modelSummaryAdjustmentNoteUi"),
            verbatimTextOutput("model_output")
          ),
          tabPanel(
            "ANOVA",
            uiOutput("modelAnovaControlsUi"),
            uiOutput("modelAnovaAdjustmentNoteUi"),
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
      ),

      tabPanel(
        "Model Explanation",
        helpText(
          "Start with the main explanation, then use the sections below for sentence support, reading guidance, and optional tutor-style help."
        ),
        selectInput(
          inputId = "modelExplanationZoom",
          label = "Explanation text size",
          choices = c("Small" = "small", "Normal" = "normal", "Large" = "large", "Presentation" = "presentation"),
          selected = "normal",
          width = "260px"
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
        h4("Developer mode"),
        passwordInput(
          inputId = "developerModePassword",
          label = "Developer mode password",
          placeholder = "Enter password to unlock developer mode"
        ),
        actionButton(
          inputId = "unlockDeveloperModeBtn",
          label = "Unlock developer mode",
          class = "btn-primary btn-sm"
        ),
        actionButton(
          inputId = "lockDeveloperModeBtn",
          label = "Lock developer mode",
          class = "btn-secondary btn-sm"
        ),
        tags$br(), tags$br(),
        textOutput("developerModeStatus"),
        helpText(
          "Developer mode exposes diagnostic controls and examples whose names begin with test. It is locked unless WMFM_DEVELOPER_MODE_PASSWORD_HASH is set and the password is entered."
        ),
        tags$hr(class = "hr-tight"),
        h4("Provider settings"),
        helpText(
          "Choose one active provider/backend. Ollama uses base URL + model and no API key. Claude uses the ANTHROPIC_API_KEY environment variable set outside WMFM."
        ),
        tags$p(
          class = "wmfm-explanation-helper-note",
          "API keys are not shown here and are never stored in the WMFM config file."
        ),
        textOutput("chatProviderStatus"),
        tags$br(),
        tags$details(tags$summary("Advanced provider diagnostics"), textOutput("providerConfigLocationStatus")),
        selectInput(
          inputId = "providerConfig_backend",
          label = "Active provider profile",
          choices = c("Ollama (local)" = "ollama", "Claude / Anthropic" = "claude", "OpenAI" = "openai", "OpenAI-compatible" = "openaiCompatible"),
          selected = "ollama"
        ),
        helpText("The controls below are Ollama-specific and apply only when Ollama is selected."),
        textInput(
          inputId = "providerConfig_ollamaBaseUrl",
          label = "Ollama base URL (Ollama only)",
          value = ""
        ),
        selectInput(
          inputId = "providerConfig_ollamaModel",
          label = "Ollama model (Ollama only)",
          choices = c("gpt-oss"),
          selected = "gpt-oss"
        ),
        checkboxInput(
          inputId = "providerConfig_ollamaThinkLow",
          label = "Default to low thinking for Ollama (Ollama only)",
          value = FALSE
        ),
        tags$div(
          style = "margin-bottom: 6px;",
          actionButton(
            inputId = "refreshOllamaModelsBtn",
            label = "Refresh available Ollama models",
            class = "btn btn-secondary btn-sm"
          )
        ),
        actionButton(
          inputId = "applyChatProviderBtn",
          label = "Apply provider",
          class = "btn-primary btn-sm"
        ),
        actionButton(
          inputId = "saveProviderConfigBtn",
          label = "Save provider config",
          class = "btn-primary btn-sm"
        ),
        actionButton(
          inputId = "resetProviderConfigBtn",
          label = "Reset provider config to defaults",
          class = "btn-secondary btn-sm"
        ),
        tags$br(), tags$br(),
        textOutput("providerConfigSaveStatus"),
        helpText(
          "Config path can be overridden with options(wmfm.config_dir = '/path')."
        )
      )

    )

  )
}
