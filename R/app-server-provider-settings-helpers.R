#' Build provider settings UI state
#'
#' @return Named list containing config-location details, provider status,
#'   and the resolved non-secret provider config values used by the settings UI.
#' @keywords internal
buildProviderSettingsState = function() {
  list(
    configLocation = describeWmfmConfigLocation(),
    providerStatus = describeWmfmProviderStatus(),
    providerConfig = resolveWmfmProviderConfig()
  )
}

#' Build provider settings status text lines
#'
#' @param settingsState Named list as returned by `buildProviderSettingsState()`.
#'
#' @return Character vector of status lines for display in the settings UI.
#' @keywords internal
buildProviderSettingsStatusLines = function(settingsState) {
  configLocation = settingsState$configLocation
  providerConfig = settingsState$providerConfig

  lines = c(
    paste0("Config file path: ", configLocation$configPath),
    paste0("Config file exists: ", if (isTRUE(configLocation$exists)) "yes" else "no"),
    paste0("Config file readable: ", if (isTRUE(configLocation$readable)) "yes" else "no"),
    paste0("Custom config directory option active: ", if (isTRUE(configLocation$customConfigDirActive)) "yes" else "no"),
    paste0("Configured provider/backend: ", providerConfig$backend %||% "ollama")
  )

  providerDetails = settingsState$providerStatus$providers
  providerNames = names(providerDetails %||% list())

  if (length(providerNames) > 0) {
    providerLines = vapply(providerNames, function(providerName) {
      details = providerDetails[[providerName]]
      availability = if (isTRUE(details$configured)) "available" else "missing credentials"
      sourceLabel = details$credentialSource %||% "unknown"
      paste0(details$provider, ": ", availability, " (source: ", sourceLabel, ")")
    }, character(1))

    lines = c(lines, "Provider credential/status summary:", paste0("- ", providerLines))
  }

  lines
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
  writeWmfmConfig(providerConfig)
}

#' Reset non-secret provider settings to package defaults
#'
#' @return Invisibly returns the config file path written by `writeWmfmConfig()`.
#' @keywords internal
resetNonSecretProviderConfig = function() {
  writeWmfmConfig(wmfmProviderDefaults())
}
