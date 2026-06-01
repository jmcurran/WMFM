#' Build provider settings UI state
#'
#' @return Named list containing config-location details, provider status,
#'   and the resolved non-secret provider config values used by the settings UI.
#' @keywords internal
buildProviderSettingsState = function() {
  list(
    configLocation = describeWmfmConfigLocation(),
    providerStatus = describeWmfmProviderStatus(),
    providerConfig = resolveWmfmProviderConfig(),
    providerProfiles = readWmfmProviderProfiles(),
    activeProfile = resolveWmfmActiveProviderProfile()
  )
}

#' Build provider settings status text lines
#'
#' @param settingsState Named list as returned by `buildProviderSettingsState()`.
#'
#' @return Character vector of status lines for display in the settings UI.
#' @keywords internal
buildProviderSettingsStatusLines = function(settingsState) {
  providerConfig = settingsState$providerConfig
  activeProfile = settingsState$activeProfile %||% list()
  selectedBackend = providerConfig$backend %||% "ollama"

  c(
    paste0("Active provider: ", activeProfile$displayName %||% selectedBackend),
    paste0("Provider type: ", activeProfile$providerType %||% selectedBackend),
    paste0("Model: ", if (identical(selectedBackend, "ollama")) providerConfig$ollamaModel %||% "gpt-oss" else (activeProfile$defaultModel %||% "default")),
    paste0("Credential: ", paste(buildProviderCredentialStatusLines(selectedBackend), collapse = " ")),
    "API key values are never stored or displayed by WMFM."
  )
}



#' Build provider credential guidance text for settings
#'
#' @param provider Character scalar provider id.
#'
#' @return Character vector of concise guidance lines for the selected provider.
#' @keywords internal
buildProviderCredentialGuidance = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  adapter = NULL
  if (isWmfmProviderSupported(providerId)) {
    adapter = getWmfmProviderAdapter(providerId)
  }

  if (identical(providerId, "claude")) {
    envVar = adapter$credentialEnvVar %||% "ANTHROPIC_API_KEY"
    return(c(
      "Claude (Anthropic) requires a credential set outside WMFM.",
      paste0("Set environment variable: ", envVar),
      "WMFM never stores or displays API key values."
    ))
  }

  if (identical(providerId, "ollama")) {
    return(c(
      "Ollama does not require an API key.",
      "Use a reachable Ollama base URL and an installed model."
    ))
  }

  if (!is.null(adapter) && isTRUE(adapter$requiresCredentials)) {
    envVar = adapter$credentialEnvVar %||% "provider-specific env var"
    return(c(
      paste0(adapter$label %||% providerId, " requires a credential set outside WMFM."),
      paste0("Set environment variable: ", envVar),
      "WMFM never stores or displays API key values."
    ))
  }

  "No credential guidance is required for this provider."
}

#' Build provider credential status lines for settings
#'
#' @param provider Character scalar provider id.
#'
#' @return Character vector describing whether credentials are detected and
#'   where they are expected.
#' @keywords internal
buildProviderCredentialStatusLines = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  credentials = resolveWmfmProviderCredentials()

  if (!providerId %in% tolower(names(credentials))) {
    return("Credential status: unknown provider.")
  }

  matchedName = names(credentials)[tolower(names(credentials)) == providerId][1]
  details = credentials[[matchedName]]
  adapter = getWmfmProviderAdapter(providerId)

  if (!isTRUE(details$requiresCredentials)) {
    return("Credential status: no credentials required.")
  }

  envVar = adapter$credentialEnvVar %||% "provider-specific env var"
  detected = if (isTRUE(details$credentialsAvailable)) "detected" else "missing"

  c(
    paste0("Credential status: ", detected, "."),
    paste0("Expected location: environment variable ", envVar, ".")
  )
}

#' Prepare non-secret provider config from settings input
#'
#' @param backend Character scalar requested backend/provider.
#' @param ollamaBaseUrl Character scalar requested Ollama base URL.
#' @param ollamaModel Character scalar requested Ollama model.
#' @param ollamaThinkLow Logical scalar low-thinking preference for Ollama.
#'
#' @return Named list containing only the non-secret config fields WMFM stores.
#' @keywords internal
prepareNonSecretProviderConfig = function(backend, ollamaBaseUrl, ollamaModel, ollamaThinkLow) {
  resolveWmfmProviderConfig(
    backend = backend,
    ollamaBaseUrl = ollamaBaseUrl,
    ollamaModel = ollamaModel,
    ollamaThinkLow = isTRUE(ollamaThinkLow)
  )
}

#' Persist settings-side non-secret provider config
#'
#' @param providerConfig Named list with non-secret provider configuration.
#'
#' @return Invisibly returns the config file path written by `writeWmfmConfig()`.
#' @keywords internal
saveNonSecretProviderConfig = function(providerConfig) {
  config = readWmfmConfig()
  providerConfig = as.list(providerConfig)

  for (fieldName in names(providerConfig)) {
    config[[fieldName]] = providerConfig[[fieldName]]
  }

  writeWmfmConfig(config)
}

#' Reset non-secret provider settings to package defaults
#'
#' @return Invisibly returns the config file path written by `writeWmfmConfig()`.
#' @keywords internal
resetNonSecretProviderConfig = function() {
  config = readWmfmConfig()
  developerModeEnabled = config$developerModeEnabled
  config = wmfmProviderDefaults()
  if (!is.null(developerModeEnabled)) {
    config$developerModeEnabled = isTRUE(developerModeEnabled)
  }

  writeWmfmConfig(config)
}
