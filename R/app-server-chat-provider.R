#' Register chat provider observers for the app server.
#'
#' Keeps Ollama model discovery and chat-provider selection wiring out of the
#' main app server orchestration function.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv App-level reactive values object.
#'
#' @return Invisibly returns `NULL`; called for its side effects.
#'
#' @keywords internal
#'
#' @importFrom shiny observe observeEvent renderText showNotification updateSelectInput
registerChatProviderObservers = function(input, output, session, rv) {
  refreshOllamaModelChoices = function(selected = NULL) {

    baseUrl = getOption(
      "wmfm.ollama_base_url",
      default = "http://corrin.stat.auckland.ac.nz:11434"
    )

    modelIds = tryCatch({
      res = ellmer::models_ollama(base_url = baseUrl)

      ids = NULL
      if (is.data.frame(res) && "id" %in% names(res)) {
        ids = as.character(res$id)
      } else if (is.atomic(res)) {
        ids = as.character(res)
      }

      ids = ids[!is.na(ids) & nzchar(ids)]
      ids = unique(ids)

      if (length(ids) == 0) {
        ids = "gpt-oss"
      }

      ids
    }, error = function(e) {
      showNotification(
        paste0("Could not retrieve Ollama models. Using current/default choices. Details: ", conditionMessage(e)),
        type = "warning",
        duration = 8
      )

      fallback = rv$availableOllamaModels %||% "gpt-oss"
      fallback = fallback[!is.na(fallback) & nzchar(fallback)]
      if (length(fallback) == 0) {
        fallback = "gpt-oss"
      }
      unique(as.character(fallback))
    })

    rv$availableOllamaModels = modelIds

    target = selected %||% rv$activeOllamaModel %||% getOption("wmfm.ollama_model", default = "gpt-oss")
    if (!(target %in% modelIds)) {
      target = if ("gpt-oss" %in% modelIds) "gpt-oss" else modelIds[1]
    }

    updateSelectInput(
      session,
      "ollama_model",
      choices = stats::setNames(modelIds, modelIds),
      selected = target
    )

    invisible(target)
  }

  observe({
    refreshOllamaModelChoices(selected = rv$activeOllamaModel %||% "gpt-oss")
  })

  observeEvent(input$refreshOllamaModelsBtn, {
    refreshOllamaModelChoices(selected = input$ollama_model %||% rv$activeOllamaModel %||% "gpt-oss")
  }, ignoreInit = TRUE)

  output$chatProviderStatus = renderText({
    buildChatProviderStatus(
      backend = rv$activeChatBackend %||% "ollama",
      ollamaModel = rv$activeOllamaModel %||% "gpt-oss",
      ollamaThinkLow = isTRUE(rv$activeOllamaThinkLow)
    )
  })

  observeEvent(input$applyChatProviderBtn, {
    requested = tolower(trimws(input$chat_provider %||% "ollama"))

    if (!requested %in% c("ollama", "claude")) {
      updateSelectInput(session, "chat_provider", selected = rv$activeChatBackend)
      showNotification(buildUnknownChatProviderMessage(), type = "error", duration = 6)
      return(NULL)
    }

    if (identical(requested, "claude")) {
      passwordOk = tryCatch(
        verifyProviderSwitchPassword(input$providerSwitchPassword %||% ""),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          FALSE
        }
      )

      if (!isTRUE(passwordOk)) {
        updateSelectInput(session, "chat_provider", selected = rv$activeChatBackend)
        session$sendInputMessage("providerSwitchPassword", list(value = ""))
        showNotification(buildClaudeProviderIncorrectPasswordMessage(), type = "error", duration = 6)
        return(NULL)
      }
    }

    selectedModel = input$ollama_model %||% rv$activeOllamaModel %||% "gpt-oss"
    availableModels = rv$availableOllamaModels %||% "gpt-oss"
    if (length(availableModels) == 0) {
      availableModels = "gpt-oss"
    }
    if (!(selectedModel %in% availableModels)) {
      selectedModel = if ("gpt-oss" %in% availableModels) "gpt-oss" else availableModels[1]
    }

    rv$activeChatBackend = requested
    if (identical(requested, "ollama")) {
      rv$activeOllamaModel = selectedModel
      rv$activeOllamaThinkLow = isTRUE(input$ollama_think_low)
    }

    session$sendInputMessage("providerSwitchPassword", list(value = ""))

    msg = buildChatProviderSetMessage(
      backend = requested,
      ollamaModel = rv$activeOllamaModel,
      ollamaThinkLow = isTRUE(rv$activeOllamaThinkLow)
    )

    showNotification(
      msg,
      type = "message",
      duration = 4
    )
  }, ignoreInit = TRUE)

  invisible(NULL)
}
