appUI = function() {
  shiny::fluidPage(
    shiny::withMathJax(),
    shiny::titlePanel("Model Builder"),

    shiny::tags$style(shiny::HTML("
      .bucket-list .rank-list {
        max-height: 8em;
        overflow-y: auto;
      }
    ")),

    # File selector
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::fileInput(
          "file",
          "Choose CSV, TXT, or RDA",
          accept = c(".csv", ".txt", ".rda", ".RData")
        )
      )
    ),

    shiny::hr(),

    # Buckets
    shiny::h4("Assign variables"),
    shiny::uiOutput("var_buckets"),

    shiny::hr(),

    # Tabs
    shiny::tabsetPanel(
      id = "main_tabs",

      shiny::tabPanel(
        "Model",
        shiny::h4("Model type"),
        shiny::radioButtons(
          "model_type",
          label = NULL,
          choices = c(
            "Linear regression" = "lm",
            "Logistic regression (binomial, logit)" = "logistic",
            "Poisson regression (log link)" = "poisson"
          ),
          selected = "lm"
        ),

        shiny::h4("Response"),
        shiny::uiOutput("response_picker"),

        shiny::h4("Model formula"),
        shiny::textInput("formula_text", label = NULL, value = "", width = "100%"),
        shiny::verbatimTextOutput("formula_status"),

        shiny::br(),
        shiny::actionButton("fit_btn", "Fit model"),
        shiny::actionButton("reset_btn", "Reset model"),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::h4("Fitted equations"),
            shiny::uiOutput("model_equations")
          ),
          shiny::column(
            6,
            shiny::h4("Model summary"),
            shiny::verbatimTextOutput("model_output")
          )
        ),

        shiny::hr(), shiny::hr(),

        shiny::h4("Model formula"),
        shiny::uiOutput("model_formula"),

        shiny::h4("Model explanation"),
        shiny::uiOutput("model_explanation")
      ),

      shiny::tabPanel(
        "Plot",
        shiny::h4("Data and fitted model"),
        shiny::plotOutput("model_plot"),
        shiny::helpText(
          "The plot shows the observed data and the fitted model ",
          "against one numeric predictor (x-axis), optionally separated by a factor."
        )
      )
    )
  )
}
