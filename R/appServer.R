appServer = function(input, output, session) {
  `%||%` = function(x, y) {
    if (is.null(x)) y else x
  }

  chat_provider = get_chat_provider()

  rv = shiny::reactiveValues(
    data = NULL,
    all_vars = character(0),
    auto_formula = "",
    model_equations = NULL,
    model_explanation = NULL,
    bucket_group_id = 0
  )

  model_fit = shiny::reactiveVal(NULL)

  # --- all your existing server body, unchanged, but with shiny:: prefixes ---
  # e.g. output$model_plot = shiny::renderPlot({ ... })
  # shiny::observeEvent(...)
  # shiny::showNotification(...)
  # shiny::withProgress(...)

  # (Mechanically move your whole server body into here.)
}
