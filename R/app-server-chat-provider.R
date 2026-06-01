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
#' @importFrom shiny observe observeEvent renderText showNotification updateCheckboxInput updateSelectInput updateTextInput showModal modalDialog tags
registerChatProviderObservers = function(input, output, session, rv) {
  resolveSelectedProvider = function() {
    requested = tolower(trimws(input$providerConfig_backend %||% rv$activeChatBackend %||% wmfmProviderDefaults()$backend))
    if (!isWmfmProviderSupported(requested)) {
      return(wmfmProviderDefaults()$backend)
    }
    requested
  }

  syncProviderSpecificControlState = function(provider) {
    isOllama = identical(provider, "ollama")
    session$sendInputMessage("providerConfig_ollamaBaseUrl", list(disabled = !isOllama))
    session$sendInputMessage("providerConfig_ollamaModel", list(disabled = !isOllama))
    session$sendInputMessage("providerConfig_ollamaThinkLow", list(disabled = !isOllama))
    session$sendInputMessage("refreshOllamaModelsBtn", list(disabled = !isOllama))
  }


  showProviderConfigurationMessage = function(provider, providerConfig) {
    provider = tolower(trimws(as.character(provider %||% "")))

    if (identical(provider, "ollama")) {
      if (!isTRUE(isWmfmProviderReadyForStartup(providerConfig))) {
        showModal(
          modalDialog(
            title = "Configure Ollama",
            tags$p("Ollama is selected, but WMFM does not yet have enough local Ollama information to use it."),
            tags$p("Set the Ollama base URL and model, make sure Ollama is running, then refresh the available models if needed."),
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
      return(invisible(NULL))
    }

    if (!isTRUE(hasWmfmProviderCredentials(provider))) {
      guidanceLines = buildProviderCredentialGuidance(provider)
      showNotification(
        paste(guidanceLines, collapse = "\n"),
        type = "warning",
        duration = 10
      )
      return(invisible(NULL))
    }

    invisible(NULL)
  }

  refreshOllamaModelChoices = function(selected = NULL) {
    activeProvider = resolveSelectedProvider()
    providerConfig = resolveWmfmProviderConfig()

    if (!identical(activeProvider, "ollama")) {
      return(invisible(rv$activeOllamaModel %||% providerConfig$ollamaModel))
    }

    if (!isWmfmProviderReadyForStartup(providerConfig)) {
      fallbackModel = rv$activeOllamaModel %||% providerConfig$ollamaModel %||% wmfmProviderDefaults()$ollamaModel
      fallbackModel = fallbackModel[!is.na(fallbackModel) & nzchar(fallbackModel)]
      if (length(fallbackModel) == 0) {
        fallbackModel = wmfmProviderDefaults()$ollamaModel
      }
      rv$availableOllamaModels = unique(as.character(fallbackModel))
      return(invisible(rv$availableOllamaModels[1]))
    }

    baseUrl = providerConfig$ollamaBaseUrl

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
        ids = wmfmProviderDefaults()$ollamaModel
      }

      ids
    }, error = function(e) {
      showNotification(
        paste0("Could not retrieve Ollama models. Using current/default choices. Details: ", conditionMessage(e)),
        type = "warning",
        duration = 8
      )

      fallback = rv$availableOllamaModels %||% wmfmProviderDefaults()$ollamaModel
      fallback = fallback[!is.na(fallback) & nzchar(fallback)]
      if (length(fallback) == 0) {
        fallback = wmfmProviderDefaults()$ollamaModel
      }
      unique(as.character(fallback))
    })

    rv$availableOllamaModels = modelIds

    target = selected %||% rv$activeOllamaModel %||% resolveWmfmProviderConfig()$ollamaModel
    if (!(target %in% modelIds)) {
      defaultModel = wmfmProviderDefaults()$ollamaModel
      target = if (defaultModel %in% modelIds) defaultModel else modelIds[1]
    }

    updateSelectInput(
      session,
      "providerConfig_ollamaModel",
      choices = stats::setNames(modelIds, modelIds),
      selected = target
    )

    invisible(target)
  }

  observe({
    selectedProvider = resolveSelectedProvider()
    syncProviderSpecificControlState(selectedProvider)
    if (identical(selectedProvider, "ollama") && isWmfmProviderReadyForStartup(resolveWmfmProviderConfig())) {
      refreshOllamaModelChoices(selected = rv$activeOllamaModel %||% wmfmProviderDefaults()$ollamaModel)
    }
  })

  session$onFlushed(function() {
    if (!isWmfmProviderReadyForStartup(resolveWmfmProviderConfig())) {
      showModal(
        modalDialog(
          title = "Configure an AI provider",
          tags$p(buildMissingProviderStartupMessage()),
          tags$p("After configuring a provider, restart WMFM or use Settings to select the provider."),
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  }, once = TRUE)

  observeEvent(input$refreshOllamaModelsBtn, {
    if (!identical(resolveSelectedProvider(), "ollama")) {
      showNotification(
        "Model discovery is only available for Ollama.",
        type = "warning",
        duration = 6
      )
      return(NULL)
    }
    refreshOllamaModelChoices(selected = input$providerConfig_ollamaModel %||% rv$activeOllamaModel %||% wmfmProviderDefaults()$ollamaModel)
  }, ignoreInit = TRUE)

  output$chatProviderStatus = renderText({
    buildChatProviderStatus(
      backend = rv$activeChatBackend %||% wmfmProviderDefaults()$backend,
      ollamaModel = rv$activeOllamaModel %||% wmfmProviderDefaults()$ollamaModel,
      ollamaThinkLow = isTRUE(rv$activeOllamaThinkLow)
    )
  })


  output$providerConfigLocationStatus = renderText({
    settingsState = buildProviderSettingsState()
    paste(buildProviderSettingsStatusLines(settingsState), collapse = "\n")
  })

  output$providerConfigSaveStatus = renderText({
    rv$providerConfigSaveStatus %||% ""
  })

  observe({
    resolvedConfig = resolveWmfmProviderConfig()

    updateSelectInput(
      session,
      "providerConfig_backend",
      selected = resolvedConfig$backend
    )

    updateTextInput(
      session,
      "providerConfig_ollamaBaseUrl",
      value = resolvedConfig$ollamaBaseUrl
    )

    updateSelectInput(
      session,
      "providerConfig_ollamaModel",
      selected = resolvedConfig$ollamaModel
    )

    updateCheckboxInput(
      session,
      "providerConfig_ollamaThinkLow",
      value = isTRUE(resolvedConfig$ollamaThinkLow)
    )
  })


  observeEvent(input$providerConfig_backend, {
    requested = tolower(trimws(input$providerConfig_backend %||% wmfmProviderDefaults()$backend))
    if (!isWmfmProviderSupported(requested)) {
      updateSelectInput(session, "providerConfig_backend", selected = rv$activeChatBackend)
      showNotification(buildUnknownChatProviderMessage(), type = "error", duration = 6)
      return(NULL)
    }

    selectedModel = input$providerConfig_ollamaModel %||% rv$activeOllamaModel %||% wmfmProviderDefaults()$ollamaModel
    selectedThinkLow = isTRUE(input$providerConfig_ollamaThinkLow)
    providerConfig = prepareNonSecretProviderConfig(
      backend = requested,
      ollamaBaseUrl = input$providerConfig_ollamaBaseUrl,
      ollamaModel = selectedModel,
      ollamaThinkLow = selectedThinkLow
    )

    rv$activeChatBackend = requested
    syncProviderSpecificControlState(requested)

    if (identical(requested, "ollama")) {
      rv$activeOllamaModel = selectedModel
      rv$activeOllamaThinkLow = selectedThinkLow
    }

    saveNonSecretProviderConfig(providerConfig)
    showProviderConfigurationMessage(requested, providerConfig)

    if (identical(requested, "ollama") && isWmfmProviderReadyForStartup(providerConfig)) {
      refreshOllamaModelChoices(selected = selectedModel)
    }
  }, ignoreInit = TRUE, priority = 90)


  observeEvent({
    list(
      input$providerConfig_ollamaBaseUrl,
      input$providerConfig_ollamaModel,
      input$providerConfig_ollamaThinkLow
    )
  }, {
    requested = resolveSelectedProvider()

    if (identical(requested, "ollama")) {
      rv$activeOllamaModel = input$providerConfig_ollamaModel %||% rv$activeOllamaModel %||% wmfmProviderDefaults()$ollamaModel
      rv$activeOllamaThinkLow = isTRUE(input$providerConfig_ollamaThinkLow)
    }

    saveNonSecretProviderConfig(prepareNonSecretProviderConfig(
      backend = requested,
      ollamaBaseUrl = input$providerConfig_ollamaBaseUrl,
      ollamaModel = input$providerConfig_ollamaModel %||% rv$activeOllamaModel %||% wmfmProviderDefaults()$ollamaModel,
      ollamaThinkLow = isTRUE(input$providerConfig_ollamaThinkLow)
    ))
  }, ignoreInit = TRUE, priority = 80)

  observeEvent(input$saveProviderConfigBtn, {
    configToSave = prepareNonSecretProviderConfig(
      backend = input$providerConfig_backend,
      ollamaBaseUrl = input$providerConfig_ollamaBaseUrl,
      ollamaModel = input$providerConfig_ollamaModel,
      ollamaThinkLow = isTRUE(input$providerConfig_ollamaThinkLow)
    )

    if (!isTRUE(hasWmfmProviderCredentials(configToSave$backend))) {
      statusLines = buildProviderCredentialStatusLines(configToSave$backend)
      rv$providerConfigSaveStatus = paste(
        c("Provider config was not saved because required credentials are missing.", statusLines),
        collapse = " "
      )
      showNotification(
        paste(c(rv$providerConfigSaveStatus, buildProviderCredentialGuidance(configToSave$backend)), collapse = "\n"),
        type = "warning",
        duration = 8
      )
      return(NULL)
    }

    savePath = saveNonSecretProviderConfig(configToSave)

    rv$activeChatBackend = configToSave$backend
    rv$activeOllamaModel = configToSave$ollamaModel
    rv$activeOllamaThinkLow = isTRUE(configToSave$ollamaThinkLow)
    rv$providerConfigSaveStatus = paste0("Provider config saved to ", savePath, ".")

    showNotification(
      "Saved non-secret provider config. API keys were not stored.",
      type = "message",
      duration = 5
    )
  }, ignoreInit = TRUE)

  observeEvent(input$resetProviderConfigBtn, {
    resetPath = resetNonSecretProviderConfig()
    defaults = wmfmProviderDefaults()

    rv$activeChatBackend = defaults$backend
    rv$activeOllamaModel = defaults$ollamaModel
    rv$activeOllamaThinkLow = isTRUE(defaults$ollamaThinkLow)
    rv$providerConfigSaveStatus = paste0("Provider config reset to defaults in ", resetPath, ".")

    updateSelectInput(session, "providerConfig_backend", selected = defaults$backend)
    updateTextInput(session, "providerConfig_ollamaBaseUrl", value = defaults$ollamaBaseUrl)
    updateSelectInput(session, "providerConfig_ollamaModel", selected = defaults$ollamaModel)
    updateCheckboxInput(session, "providerConfig_ollamaThinkLow", value = isTRUE(defaults$ollamaThinkLow))

    showNotification(
      "Reset non-secret provider config to defaults. API keys were not stored.",
      type = "message",
      duration = 5
    )
  }, ignoreInit = TRUE)


  invisible(NULL)
}
