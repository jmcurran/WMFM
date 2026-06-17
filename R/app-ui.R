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
#' @importFrom shiny br actionButton downloadButton plotOutput helpText
#' @importFrom shiny conditionalPanel selectInput div checkboxInput textOutput passwordInput
#' @importFrom shiny sidebarLayout sidebarPanel mainPanel tableOutput
#' @importFrom bslib accordion accordion_panel bs_theme
appUI = function() {
  fluidPage(
    theme = bs_theme(),
    withMathJax(),
    titlePanel("What's My Fitted Model?"),

    tags$style(HTML("
      .bucket-list .rank-list,
      .bucket-list-container .rank-list,
      .rank-list-container .rank-list {
        max-height: 8em;
        overflow-y: auto;
        margin-top: 0 !important;
        padding-top: 0 !important;
      }
      html, body {
        min-height: 100%;
        overflow-y: auto;
      }
      body {
        font-size: 90%;
      }
      .container-fluid {
        padding-bottom: 18px;
      }
      .shiny-input-container { font-size: 90%; }
      .nav-tabs > li > a { font-size: 90%; }
      pre, code { font-size: 90%; }

      h4 {
        margin-top: 12px;
        margin-bottom: 8px;
      }

      h5 {
        margin-top: 6px;
        margin-bottom: 4px;
      }

      hr {
        margin: 8px 0;
      }

      .hr-tight {
        margin: 6px 0;
      }

      .form-group {
        margin-bottom: 8px;
      }

      .radio {
        margin-top: 3px;
        margin-bottom: 3px;
      }

      .shiny-html-output,
      .shiny-text-output {
        margin-bottom: 6px;
      }

      .wmfm-ci-section-label {
        font-weight: 600;
        margin-top: 10px;
        margin-bottom: 4px;
      }

      .wmfm-ci-drilldown-box {
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 12px;
        background-color: #fcfcfc;
        margin-top: 10px;
        margin-bottom: 10px;
      }

      .wmfm-ci-secondary-note {
        color: #666;
        margin-bottom: 8px;
      }

      .wmfm-ci-collapsible-block {
        margin-top: 10px;
      }

      .wmfm-explanation-box {
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 12px;
        background-color: #fcfcfc;
        margin-top: 8px;
        white-space: normal;
      }

      .wmfm-explanation-box p {
        margin: 0 0 0.8em 0;
      }

      .wmfm-explanation-box p:last-child {
        margin-bottom: 0;
      }

      .wmfm-explanation-helper-box {
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 12px;
        background-color: #f8f9fb;
        margin-top: 10px;
        margin-bottom: 10px;
      }

      .wmfm-explanation-helper-note {
        color: #666;
        margin-bottom: 8px;
      }

      .wmfm-model-tab h5 {
        margin-top: 6px;
        margin-bottom: 4px;
      }

      .wmfm-model-tab {
        padding-bottom: 16px;
      }

      .wmfm-model-tab .help-block {
        margin-bottom: 6px;
      }

      #modelFollowupQuestion::placeholder {
        color: #9aa0a6;
        opacity: 1;
      }

      #modelFollowupQuestion::-webkit-input-placeholder {
        color: #9aa0a6;
      }

      #modelFollowupQuestion::-moz-placeholder {
        color: #9aa0a6;
        opacity: 1;
      }

      #model_question_accordion .accordion-button,
      #model_question_accordion .accordion-header button,
      #model_question_accordion .panel-title a {
        padding-top: 8px;
        padding-bottom: 8px;
        min-height: 36px;
      }

      #model_question_accordion .accordion-body,
      #model_question_accordion .panel-body {
        padding-top: 8px;
        padding-bottom: 8px;
      }

      #model_question_accordion {
        margin-bottom: 4px;
      }

      .wmfm-model-tab .form-group {
        margin-bottom: 8px;
      }

      .wmfm-model-tab .hr-tight {
        margin: 6px 0;
      }

      .wmfm-model-tab #formula_text {
        margin-bottom: 4px;
      }

      .wmfm-model-tab #formula_status {
        margin-top: 4px;
        margin-bottom: 0;
        min-height: 1.4em;
      }

      .wmfm-formula-status {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 0.9em;
        font-weight: 600;
      }

      .wmfm-formula-status-ok {
        background-color: #e8f5e9;
        color: #1b5e20;
        border: 1px solid #c8e6c9;
      }

      .wmfm-formula-status-error {
        background-color: #ffebee;
        color: #b71c1c;
        border: 1px solid #ffcdd2;
      }

      .wmfm-model-tab .checkbox {
        margin-top: 6px;
        margin-bottom: 6px;
      }

      .wmfm-optional-controls-row .wmfm-optional-control-btn {
        display: flex;
        align-items: center;
        min-height: 34px;
      }

      .wmfm-optional-controls-row .wmfm-optional-control-btn .btn {
        margin-bottom: 0;
        display: inline-flex;
        align-items: center;
      }

      .wmfm-model-compact-action-btn {
        padding: 4px 10px;
        font-size: 12px;
        line-height: 1.5;
        border-radius: 3px;
        min-height: 30px;
      }

      .bucket-list .panel,
      .bucket-list .card,
      .bucket-list-container .panel,
      .bucket-list-container .card,
      .rank-list-container .panel,
      .rank-list-container .card {
        margin-top: 0 !important;
        padding-top: 0 !important;
      }

      .bucket-list .panel-heading,
      .bucket-list .card-header,
      .bucket-list-container .panel-heading,
      .bucket-list-container .card-header,
      .rank-list-container .panel-heading,
      .rank-list-container .card-header {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
        padding: 2px 10px 0 10px !important;
      }

      .bucket-list .panel-body,
      .bucket-list .card-body,
      .bucket-list-container .panel-body,
      .bucket-list-container .card-body,
      .rank-list-container .panel-body,
      .rank-list-container .card-body {
        margin-top: 0 !important;
        padding: 2px 10px 0 10px !important;
      }

      div.rank-list-container.default-sortable,
      .bucket-list .rank-list-container,
      .bucket-list-container .rank-list-container,
      .rank-list-container,
      .default-sortable.rank-list-container {
        margin-top: 0 !important;
        padding: 0 2px !important;
        padding-top: 0 !important;
      }

      div.rank-list-container.default-sortable > .wmfm-variable-bucket-header {
        margin-top: 0 !important;
        margin-bottom: 2px !important;
      }

      .rank-list-container > .rank-list-title,
      .default-sortable.rank-list-container > .rank-list-title,
      .bucket-list .rank-list-title,
      .bucket-list-container .rank-list-title {
        display: none !important;
        height: 0 !important;
        margin: 0 !important;
        padding: 0 !important;
      }

      .bucket-list .rank-list,
      .bucket-list-container .rank-list,
      .rank-list-container .rank-list,
      .default-sortable.rank-list {
        margin-top: 0 !important;
        padding-top: 0 !important;
      }

      .wmfm-variable-bucket-header {
        display: flex;
        align-items: center;
        gap: 34px;
        min-height: 24px;
        line-height: 1.2;
        margin: 0;
        padding: 0;
        white-space: nowrap;
        overflow: visible;
      }

      .wmfm-variable-bucket-title {
        display: inline-flex;
        align-items: center;
        min-height: 22px;
      }

      .wmfm-variable-bucket-header .btn {
        margin-top: 0;
        margin-bottom: 0;
        min-height: 24px;
        padding: 2px 10px;
        line-height: 1.2;
        vertical-align: middle;
      }

      .wmfm-optional-info-icon,
      .wmfm-optional-controls-label .fa,
      .wmfm-optional-controls-label svg,
      #interaction_label_ui .fa,
      #interaction_label_ui svg {
        margin-left: 6px;
      }

      .wmfm-optional-controls-row {
        display: flex;
        align-items: flex-start;
      }

      .wmfm-optional-controls-label {
        margin-top: 0;
        margin-bottom: 8px;
        min-height: 32px;
        display: flex;
        align-items: center;
      }

      .wmfm-optional-controls-row .form-group {
        margin-bottom: 0;
      }

      .wmfm-model-fit-buttons {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, auto));
        align-items: center;
        justify-content: start;
        gap: 8px;
      }

      .wmfm-model-fit-buttons .btn {
        margin-bottom: 0;
        width: auto;
      }

      .wmfm-model-fit-actions {
        margin-top: 25px;
      }

      @media (max-width: 767px) {
        .wmfm-model-fit-buttons {
          grid-template-columns: 1fr;
        }
      }

      .wmfm-data-context-control {
        display: flex;
        align-items: center;
        justify-content: flex-end;
        min-height: 34px;
      }

      .wmfm-data-context-control .btn {
        margin-bottom: 0;
        display: inline-flex;
        align-items: center;
      }

      .wmfm-data-context-inline-control {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: flex-end;
        gap: 8px;
        flex-wrap: nowrap;
        white-space: nowrap;
      }

      .wmfm-data-context-inline-control > * {
        flex: 0 0 auto;
      }

      .wmfm-data-context-inline-control .wmfm-formula-status {
        margin-left: 0;
        white-space: nowrap;
      }

      .wmfm-developer-mode-toggle-row {
        display: inline-flex;
        align-items: center;
        gap: 12px;
        margin-bottom: 8px;
      }

      .wmfm-developer-mode-toggle-label {
        font-weight: 600;
        margin-bottom: 0;
      }

      .wmfm-developer-mode-switch {
        position: relative;
        display: inline-block;
        width: 56px;
        height: 30px;
        margin-bottom: 0;
      }

      .wmfm-developer-mode-switch input[type=checkbox] {
        position: absolute;
        opacity: 0;
        width: 0;
        height: 0;
      }

      .wmfm-developer-mode-slider {
        position: absolute;
        cursor: pointer;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background-color: #d9534f;
        border-radius: 999px;
        transition: background-color 0.18s ease-in-out;
        box-shadow: inset 0 1px 2px rgba(0, 0, 0, 0.20);
      }

      .wmfm-developer-mode-slider::before {
        content: '';
        position: absolute;
        height: 24px;
        width: 24px;
        left: 3px;
        bottom: 3px;
        background-color: #ffffff;
        border-radius: 50%;
        transition: transform 0.18s ease-in-out;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.35);
      }

      .wmfm-developer-mode-switch input[type=checkbox]:checked + .wmfm-developer-mode-slider {
        background-color: #2e7d32;
      }

      .wmfm-developer-mode-switch input[type=checkbox]:checked + .wmfm-developer-mode-slider::before {
        transform: translateX(26px);
      }

      .wmfm-developer-mode-switch input[type=checkbox]:focus + .wmfm-developer-mode-slider {
        outline: 2px solid #4d90fe;
        outline-offset: 2px;
      }

      .wmfm-provider-settings-heading {
        display: flex;
        align-items: center;
        gap: 8px;
      }

      .wmfm-provider-settings-info {
        display: inline-block;
        position: relative;
        font-size: 0.75em;
      }

      .wmfm-provider-settings-info summary {
        cursor: pointer;
        list-style: none;
        display: inline-flex;
        align-items: center;
      }

      .wmfm-provider-settings-info summary::-webkit-details-marker {
        display: none;
      }

      .wmfm-provider-settings-info-body {
        position: absolute;
        z-index: 1000;
        top: 1.8em;
        left: 0;
        width: 360px;
        max-width: 80vw;
        padding: 10px 12px;
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        background-color: #ffffff;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.18);
        color: #333333;
        font-size: 0.6em;
        font-weight: 400;
        line-height: 1.35;
      }

      .wmfm-provider-settings-info-body p {
        margin: 0 0 6px 0;
      }

      .wmfm-provider-settings-info-body p:last-child {
        margin-bottom: 0;
      }


      .wmfm-model-plots-heading {
        display: flex;
        align-items: center;
        gap: 8px;
      }

      .wmfm-model-plots-info {
        display: inline-block;
        position: relative;
        font-size: 0.75em;
      }

      .wmfm-model-plots-info summary {
        cursor: pointer;
        list-style: none;
        display: inline-flex;
        align-items: center;
      }

      .wmfm-model-plots-info summary::-webkit-details-marker {
        display: none;
      }

      .wmfm-model-plots-info-body {
        position: absolute;
        z-index: 1000;
        top: 1.8em;
        left: 0;
        width: 360px;
        max-width: 80vw;
        padding: 10px 12px;
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        background-color: #ffffff;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.18);
        color: #333333;
        font-size: 0.6em;
        font-weight: 400;
        line-height: 1.35;
      }

      .wmfm-model-plots-info-body p {
        margin: 0 0 6px 0;
      }

      .wmfm-model-plots-info-body p:last-child {
        margin-bottom: 0;
      }

      .wmfm-model-plot-note {
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 12px;
        background-color: #fcfcfc;
        margin-top: 10px;
        margin-bottom: 10px;
      }

      .wmfm-model-plot-note-heading {
        font-weight: 600;
        margin-bottom: 8px;
      }

      .wmfm-model-plot-note dt {
        margin-top: 6px;
      }

      .wmfm-model-plot-note dd {
        margin-left: 0;
        margin-bottom: 6px;
      }

      .tab-content {
        overflow: visible;
      }
        ")),

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
        fluidRow(
          column(
            6,
            selectInput(
              "exampleName",
              "Choose an example:",
              choices = c("Loading examples..." = "")
            ),
            actionButton(
              "loadExampleBtn",
              "Load example",
              class = "btn btn-secondary"
            )
          ),
          column(
            6,
            uiOutput("exampleMetadataUi")
          )
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
            class = "wmfm-response-context-row",
            column(
              width = 8,
              uiOutput("response_picker")
            ),
            column(
              width = 4,
              div(
                class = "wmfm-data-context-control",
                style = "padding-top: 27px;",
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
                placeholder = "Optional: ask a follow-up question about this fitted model."
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
              h5(
                class = "wmfm-optional-controls-label",
                "Response transformation ",
                tags$span(
                  class = "wmfm-optional-info-icon",
                  icon("circle-info"),
                  title = paste(
                    "Choose how explanations should use an invertible transformation of the response variable.",
                    "Both keeps the model scale and adds the original response scale when available.",
                    "Model scale uses only the scale used to fit the model.",
                    "Original response scale uses the back-transformed scale when available."
                  ),
                  style = "cursor: help;"
                )
              ),
              selectInput(
                "responseTransformationMode",
                label = NULL,
                choices = c(
                  "Both" = "both",
                  "Model scale" = "model",
                  "Original response scale" = "original"
                ),
                selected = "both",
                width = "100%"
              )
            ),
            column(
              width = 8,
              uiOutput("interaction_label_ui"),
              uiOutput("interaction_ui")
            )
          ),

          tags$hr(class = "hr-tight"),

          h5("Model type and model fitting"),
          fluidRow(
            class = "wmfm-model-fit-row",
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
                class = "wmfm-model-fit-buttons wmfm-model-fit-actions",
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
        "Model plots",
        h4(
          class = "wmfm-model-plots-heading",
          tags$span("Model plots"),
          tags$details(
            class = "wmfm-model-plots-info",
            tags$summary(
              icon("circle-info"),
              tags$span(class = "sr-only", "Model plots information")
            ),
            tags$div(
              class = "wmfm-model-plots-info-body",
              tags$p(
                "These plots help you notice whether the fitted model is missing obvious structure."
              )
            )
          )
        ),
        uiOutput("modelPlotTypeUi"),
        uiOutput("modelPlotSmoothTrendUi"),
        plotOutput("modelPlotsPlot", height = "360px"),
        downloadButton(
          outputId = "modelPlotsDownload",
          label = "Download current model plot",
          class = "wmfm-model-plot-download"
        ),
        uiOutput("modelPlotTeachingNoteUi")
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
        "Settings",
        if (isDeveloperModeUiEnabled()) {
          tagList(
            h4("Developer mode"),
            div(
              class = "wmfm-developer-mode-toggle-row",
              tags$span(
                class = "wmfm-developer-mode-toggle-label",
                "Developer mode:"
              ),
              tags$label(
                class = "wmfm-developer-mode-switch",
                tags$input(
                  id = "developerModeToggle",
                  type = "checkbox",
                  `aria-label` = "Developer mode"
                ),
                tags$span(class = "wmfm-developer-mode-slider")
              )
            ),
            tags$hr(class = "hr-tight")
          )
        },
        h4(
          class = "wmfm-provider-settings-heading",
          tags$span("Provider settings"),
          tags$details(
            class = "wmfm-provider-settings-info",
            tags$summary(
              icon("circle-info"),
              tags$span(class = "sr-only", "Provider settings information")
            ),
            tags$div(
              class = "wmfm-provider-settings-info-body",
              tags$p(
                "Choose the AI provider WMFM should use. Add or edit providers if you are running WMFM on your own computer."
              ),
              tags$p(
                "A provider stores the connection details for a local Ollama service or a hosted service such as Claude. For Claude, the administrator route is ANTHROPIC_API_KEY."
              ),
              tags$p(
                "API keys are not shown here and are never stored by WMFM. Credential guidance is shown in a separate dialog so API-key setup is not front and centre."
              )
            )
          )
        ),
        selectInput(
          inputId = "providerConfig_backend",
          label = "Active provider",
          choices = buildProviderProfileChoices(),
          selected = resolveWmfmActiveProviderProfile()$profileId
        ),
        tableOutput("providerRegistryTable"),
        tags$div(
          class = "wmfm-provider-registry-actions",
          actionButton(
            inputId = "addProviderProfileBtn",
            label = "+",
            title = "Add provider",
            class = "btn-secondary btn-sm"
          ),
          actionButton(
            inputId = "removeProviderProfileBtn",
            label = "-",
            title = "Remove the active provider",
            class = "btn-secondary btn-sm"
          ),
          actionButton(
            inputId = "showProviderSetupBtn",
            label = "Setup selected provider",
            class = "btn-secondary btn-sm"
          )
        ),
        helpText("In a deployed WMFM app, available providers and models are controlled by the installer."),
        tags$details(
          tags$summary("Advanced provider diagnostics"),
          textOutput("providerConfigLocationStatus")
        ),
        tags$details(
          tags$summary("Advanced Ollama configuration"),
          helpText("Only change these settings when you are running or administering an Ollama service."),
          textInput(
            inputId = "providerConfig_ollamaBaseUrl",
            label = "Ollama base URL",
            value = ""
          ),
          selectInput(
            inputId = "providerConfig_ollamaModel",
            label = "Ollama model",
            choices = c("gpt-oss"),
            selected = "gpt-oss"
          ),
          checkboxInput(
            inputId = "providerConfig_ollamaThinkLow",
            label = "Default to low thinking for Ollama",
            value = FALSE
          ),
          tags$div(
            style = "margin-bottom: 6px;",
            actionButton(
              inputId = "refreshOllamaModelsBtn",
              label = "Refresh available Ollama models",
              class = "btn btn-secondary btn-sm"
            )
          )
        ),
        textOutput("providerConfigSaveStatus")
      )

    )

  )
}
