#' Register the contrasts module
#'
#' @param input,output,session Shiny server objects
#' @param rv reactiveValues() list (must contain chatProvider, contrastLlmCache)
#' @param modelFit reactive returning fitted model
#' @param modelFrame reactive returning model.frame(model)
#'
#' @export
registerContrastsModule = function(input, output, session, rv, modelFit, modelFrame) {

  # ---- UI ----
  output$contrastUi = renderUI({
    m = modelFit()
    req(m)

    mf = modelFrame()
    req(mf)

    factorVars = names(Filter(is.factor, mf))
    req(length(factorVars) >= 1)

    factorVar = input$contrastFactor %||% factorVars[1]
    levs = levels(mf[[factorVar]])

    tagList(
      h4("Contrasts"),

      selectInput(
        "contrastFactor",
        "Factor",
        choices = factorVars,
        selected = factorVar
      ),

      radioButtons(
        "contrastMode",
        "Contrast type",
        choices = c(
          "Compare pairs of levels" = "pairs",
          "Average of groups" = "avg",
          "Custom contrast (advanced)" = "custom"
        ),
        selected = "pairs"
      ),

      conditionalPanel(
        "input.contrastMode == 'pairs'",
        contrastsPairsUi(levs)
      ),

      conditionalPanel(
        "input.contrastMode == 'avg'",
        contrastsAvgUi(levs)
      ),

      conditionalPanel(
        "input.contrastMode == 'custom'",
        contrastsCustomUi(levs)
      ),

      hr(),
      verbatimTextOutput("contrastResult")
    )
  })

  output$contrastResult = renderText({
    rv$contrastResultText %||% ""
  })

  # ---- Pairwise add/remove ----
  observeEvent(input$addContrastBtn, {
    a = input$contrastLevel1
    b = input$contrastLevel2
    req(a, b)
    validate(need(a != b, "Choose two different levels."))

    levs = levels(modelFrame()[[input$contrastFactor]])
    ord = order(match(c(a, b), levs))
    pair = c(a, b)[ord]
    label = paste(pair, collapse = " - ")

    rv$contrastPairs = unique(c(rv$contrastPairs, label))
  })

  observeEvent(input$removeContrastBtn, {
    rv$contrastPairs = setdiff(rv$contrastPairs, input$contrastList)
  })

  observe({
    updateSelectInput(
      session,
      "contrastList",
      choices = rv$contrastPairs,
      selected = character(0)
    )
  })

  # ---- Compute ----
  observeEvent(input$computeContrastsBtn, {
    m = modelFit()
    mf = modelFrame()
    req(m, mf)

    results = character(0)

    if (input$contrastMode == "pairs") {
      for (lab in rv$contrastPairs) {
        res = computePairwiseContrast(m, mf, input$contrastFactor, lab)
        results = c(results, formatContrast(res, m, rv))
      }
    }

    if (input$contrastMode == "avg") {
      res = computeAvgContrast(
        m, mf, input$contrastFactor,
        input$avgLeft, input$avgRight
      )
      results = c(results, formatContrast(res, m, rv))
    }

    if (input$contrastMode == "custom") {
      res = computeCustomContrast(
        m, mf, input$contrastFactor,
        input$customWeights
      )
      results = c(results, formatContrast(res, m, rv))
    }

    rv$contrastResultText = paste(results, collapse = "\n\n")
  })
}
