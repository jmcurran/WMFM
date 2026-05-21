#' WMFM default chat provider configuration
#'
#' Returns the package defaults used when provider-related options or app-state
#' values are missing.
#'
#' @return Named list with default backend, Ollama base URL, Ollama model,
#'   and Ollama low-thinking flag.
#' @keywords internal
wmfmProviderDefaults = function() {
  list(
    backend = "ollama",
    ollamaBaseUrl = "http://corrin.stat.auckland.ac.nz:11434",
    ollamaModel = "gpt-oss",
    ollamaThinkLow = FALSE
  )
}

#' WMFM local configuration directory
#'
#' Returns the directory used for non-secret local WMFM configuration.
#'
#' @return Character scalar path.
#' @keywords internal
wmfmConfigDir = function() {
  optionDir = getOption("wmfm.config_dir", default = NULL)
  if (!is.null(optionDir)) {
    resolvedOptionDir = trimws(as.character(optionDir))
    if (nzchar(resolvedOptionDir)) {
      return(path.expand(resolvedOptionDir))
    }
  }

  path.expand(file.path("~", ".wmfm"))
}

#' WMFM local configuration file path
#'
#' @return Character scalar path to local configuration file.
#' @keywords internal
wmfmConfigPath = function() {
  file.path(wmfmConfigDir(), "config.json")
}

#' Read WMFM local configuration
#'
#' Reads persisted, non-secret local defaults for provider settings.
#'
#' @return Named list with recognized non-secret fields when present.
#' @keywords internal
readWmfmConfig = function() {
  configPath = wmfmConfigPath()
  if (!file.exists(configPath)) {
    return(list())
  }

  parsed = tryCatch(
    jsonlite::read_json(path = configPath, simplifyVector = TRUE),
    error = function(err) {
      NULL
    }
  )

  if (is.null(parsed) || !is.list(parsed)) {
    return(list())
  }

  recognizedFields = c(
    "backend",
    "ollamaBaseUrl",
    "ollamaModel",
    "ollamaThinkLow"
  )

  parsed[intersect(names(parsed), recognizedFields)]
}

#' Write WMFM local configuration
#'
#' Persists non-secret provider defaults for local reuse.
#'
#' @param config Named list with provider defaults to persist.
#'
#' @return Invisibly returns the path that was written.
#' @keywords internal
writeWmfmConfig = function(config = list()) {
  config = as.list(config)

  allowedFields = c(
    "backend",
    "ollamaBaseUrl",
    "ollamaModel",
    "ollamaThinkLow"
  )
  secretFieldNames = c("anthropicApiKey", "apiKey", "ANTHROPIC_API_KEY")

  kept = config[intersect(names(config), allowedFields)]
  kept = kept[setdiff(names(kept), secretFieldNames)]

  configPath = wmfmConfigPath()
  dir.create(dirname(configPath), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x = kept, path = configPath, auto_unbox = TRUE, pretty = TRUE)

  invisible(configPath)
}

#' Resolve effective WMFM provider configuration
#'
#' Computes the effective provider configuration from explicit values and
#' options while preserving WMFM defaults.
#'
#' @param backend Optional backend override.
#' @param ollamaBaseUrl Optional Ollama base URL override.
#' @param ollamaModel Optional Ollama model override.
#' @param ollamaThinkLow Optional low-thinking override.
#'
#' @return Named list with normalized provider config.
#' @keywords internal
resolveWmfmProviderConfig = function(backend = NULL,
                                     ollamaBaseUrl = NULL,
                                     ollamaModel = NULL,
                                     ollamaThinkLow = NULL) {
  defaults = wmfmProviderDefaults()
  localConfig = readWmfmConfig()

  resolvedBackend = tolower(trimws(backend %||%
    getOption("wmfm.chat_backend", default = NULL) %||%
    localConfig$backend %||%
    defaults$backend))
  if (!nzchar(resolvedBackend)) {
    resolvedBackend = defaults$backend
  }

  resolvedBaseUrl = trimws(as.character(ollamaBaseUrl %||%
    getOption("wmfm.ollama_base_url", default = NULL) %||%
    localConfig$ollamaBaseUrl %||%
    defaults$ollamaBaseUrl))
  if (!nzchar(resolvedBaseUrl)) {
    resolvedBaseUrl = defaults$ollamaBaseUrl
  }

  resolvedModel = trimws(as.character(ollamaModel %||%
    getOption("wmfm.ollama_model", default = NULL) %||%
    localConfig$ollamaModel %||%
    defaults$ollamaModel))
  if (!nzchar(resolvedModel)) {
    resolvedModel = defaults$ollamaModel
  }

  resolvedThinkLow = isTRUE(ollamaThinkLow %||%
    getOption("wmfm.ollama_think_low", default = NULL) %||%
    localConfig$ollamaThinkLow %||%
    defaults$ollamaThinkLow)

  list(
    backend = resolvedBackend,
    ollamaBaseUrl = resolvedBaseUrl,
    ollamaModel = resolvedModel,
    ollamaThinkLow = resolvedThinkLow
  )
}

#' Test whether Claude credentials are configured
#'
#' @return Logical scalar; `TRUE` when `ANTHROPIC_API_KEY` is set.
#' @keywords internal
hasClaudeApiKey = function() {
  nzchar(Sys.getenv("ANTHROPIC_API_KEY", unset = ""))
}

#' Resolve WMFM provider credentials
#'
#' Resolves non-secret credential metadata for supported chat providers without
#' exposing secret values.
#'
#' @return Named list keyed by provider id. Each provider entry contains
#'   `provider`, `requiresCredentials`, `credentialsAvailable`,
#'   `credentialSource`, and `localOnly` fields.
#' @keywords internal
resolveWmfmProviderCredentials = function() {
  claudeKeyPresent = hasClaudeApiKey()

  list(
    ollama = list(
      provider = "ollama",
      requiresCredentials = FALSE,
      credentialsAvailable = TRUE,
      credentialSource = "none-required",
      localOnly = TRUE
    ),
    claude = list(
      provider = "claude",
      requiresCredentials = TRUE,
      credentialsAvailable = isTRUE(claudeKeyPresent),
      credentialSource = if (isTRUE(claudeKeyPresent)) "env:ANTHROPIC_API_KEY" else "missing",
      localOnly = FALSE
    ),
    openai = list(
      provider = "openai",
      requiresCredentials = TRUE,
      credentialsAvailable = FALSE,
      credentialSource = "not-configured",
      localOnly = FALSE
    ),
    openaiCompatible = list(
      provider = "openaiCompatible",
      requiresCredentials = FALSE,
      credentialsAvailable = TRUE,
      credentialSource = "future-ready",
      localOnly = TRUE
    )
  )
}

#' Test whether WMFM provider credentials are available
#'
#' @param provider Character scalar provider id.
#'
#' @return Logical scalar indicating whether required credentials are available.
#' @keywords internal
hasWmfmProviderCredentials = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  credentials = resolveWmfmProviderCredentials()
  if (!providerId %in% tolower(names(credentials))) {
    return(FALSE)
  }

  matchedName = names(credentials)[tolower(names(credentials)) == providerId][1]
  isTRUE(credentials[[matchedName]]$credentialsAvailable)
}

#' Describe WMFM provider status
#'
#' Returns deterministic, printable provider metadata and credential status
#' without including secret values.
#'
#' @return Named list with `providers` and `configuredProviders` entries.
#' @keywords internal
describeWmfmProviderStatus = function() {
  providerCredentials = resolveWmfmProviderCredentials()
  providerOrder = c("ollama", "claude", "openai", "openaiCompatible")
  providerNames = intersect(providerOrder, names(providerCredentials))

  providers = lapply(providerNames, function(providerName) {
    details = providerCredentials[[providerName]]
    list(
      provider = details$provider,
      configured = isTRUE(details$credentialsAvailable),
      requiresCredentials = isTRUE(details$requiresCredentials),
      credentialsAvailable = isTRUE(details$credentialsAvailable),
      credentialSource = details$credentialSource,
      locallyAvailable = isTRUE(details$localOnly),
      ready = isTRUE(details$credentialsAvailable) || !isTRUE(details$requiresCredentials)
    )
  })

  names(providers) = providerNames

  list(
    providers = providers,
    configuredProviders = names(providers)[vapply(providers, function(x) isTRUE(x$configured), logical(1))]
  )
}

#' Describe WMFM config location status
#'
#' Returns path and accessibility metadata for the local WMFM non-secret
#' configuration file.
#'
#' @return Named list containing config file location details.
#' @keywords internal
describeWmfmConfigLocation = function() {
  configPath = wmfmConfigPath()
  customConfigDir = getOption("wmfm.config_dir", default = NULL)
  customConfigDirActive = !is.null(customConfigDir) && nzchar(trimws(as.character(customConfigDir)))
  exists = file.exists(configPath)

  readable = FALSE
  if (isTRUE(exists)) {
    readable = isTRUE(tryCatch(file.access(configPath, mode = 4) == 0, error = function(err) FALSE))
  }

  list(
    configDir = wmfmConfigDir(),
    configPath = configPath,
    exists = isTRUE(exists),
    readable = isTRUE(readable),
    customConfigDirActive = isTRUE(customConfigDirActive)
  )
}
