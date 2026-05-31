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

#' Normalise a WMFM provider profile
#'
#' @param profile Named list describing a configured provider profile.
#'
#' @return Named list with normalised provider profile metadata.
#' @keywords internal
normaliseWmfmProviderProfile = function(profile = list()) {
  profile = as.list(profile %||% list())
  providerType = tolower(trimws(as.character(profile$providerType %||% profile$provider %||% "ollama")))
  if (!isWmfmProviderSupported(providerType)) {
    providerType = "ollama"
  }
  adapter = getWmfmProviderAdapter(providerType)
  providerType = adapter$provider
  profileId = trimws(as.character(profile$profileId %||% profile$providerId %||% providerType))
  if (!nzchar(profileId)) {
    profileId = providerType
  }
  displayName = trimws(as.character(profile$displayName %||% adapter$label %||% providerType))
  if (!nzchar(displayName)) {
    displayName = adapter$label %||% providerType
  }
  list(profileId = profileId, providerId = profileId, displayName = displayName, providerType = providerType,
    apiUrl = trimws(as.character(profile$apiUrl %||% profile$baseUrl %||% "")),
    credentialSource = trimws(as.character(profile$credentialSource %||% if (isTRUE(adapter$requiresCredentials)) "envvar" else "none")),
    credentialEnvVar = trimws(as.character(profile$credentialEnvVar %||% adapter$credentialEnvVar %||% "")),
    defaultModel = trimws(as.character(profile$defaultModel %||% "")),
    supportsModelDiscovery = isTRUE(profile$supportsModelDiscovery %||% adapter$supportsLocalDiscovery),
    supportsThinkingLevels = isTRUE(profile$supportsThinkingLevels %||% identical(providerType, "ollama")),
    enabled = !identical(profile$enabled, FALSE), active = isTRUE(profile$active))
}

#' @keywords internal
wmfmDefaultProviderProfiles = function() {
  defaults = wmfmProviderDefaults()
  list(normaliseWmfmProviderProfile(list(profileId = "ollama-local", displayName = "Ollama (local)", providerType = "ollama", apiUrl = defaults$ollamaBaseUrl, defaultModel = defaults$ollamaModel, supportsModelDiscovery = TRUE, supportsThinkingLevels = TRUE, enabled = TRUE, active = identical(defaults$backend, "ollama"))), normaliseWmfmProviderProfile(list(profileId = "claude-anthropic", displayName = "Claude / Anthropic", providerType = "claude", credentialSource = "envvar", credentialEnvVar = "ANTHROPIC_API_KEY", enabled = TRUE, active = identical(defaults$backend, "claude"))))
}

#' @keywords internal
migrateLegacyProviderConfigToProfiles = function(config = list()) {
  defaults = wmfmProviderDefaults()
  list(normaliseWmfmProviderProfile(list(profileId = "ollama-local", displayName = "Ollama (local)", providerType = "ollama", apiUrl = config$ollamaBaseUrl %||% defaults$ollamaBaseUrl, defaultModel = config$ollamaModel %||% defaults$ollamaModel, supportsModelDiscovery = TRUE, supportsThinkingLevels = TRUE, enabled = TRUE, active = identical(tolower(config$backend %||% defaults$backend), "ollama"))), normaliseWmfmProviderProfile(list(profileId = "claude-anthropic", displayName = "Claude / Anthropic", providerType = "claude", credentialSource = "envvar", credentialEnvVar = "ANTHROPIC_API_KEY", enabled = TRUE, active = identical(tolower(config$backend %||% defaults$backend), "claude"))))
}

#' @keywords internal
readWmfmProviderProfiles = function() {
  config = readWmfmConfig()
  raw = config$providerProfiles
  if (is.data.frame(raw)) {
    raw = split(raw, seq_len(nrow(raw)))
  }
  if (!is.list(raw) || length(raw) == 0) {
    return(migrateLegacyProviderConfigToProfiles(config))
  }
  lapply(raw, normaliseWmfmProviderProfile)
}

#' @keywords internal
writeWmfmProviderProfiles = function(profiles = list()) {
  normalised = lapply(profiles, normaliseWmfmProviderProfile)
  cfg = readWmfmConfig()
  cfg$providerProfiles = normalised
  writeWmfmConfig(cfg)
}

#' @keywords internal
resolveWmfmActiveProviderProfile = function(backend = NULL) {
  profiles = readWmfmProviderProfiles()
  backend = tolower(trimws(as.character(backend %||% resolveWmfmProviderConfig()$backend %||% "")))
  idx = which(vapply(profiles, function(x) identical(tolower(trimws(as.character(x$providerType %||% ""))), backend), logical(1)))[1]
  if (!is.na(idx)) {
    return(profiles[[idx]])
  }
  profiles[[1]]
}

#' @keywords internal
buildProviderProfileChoices = function(profiles = readWmfmProviderProfiles()) {
  stats::setNames(vapply(profiles, function(x) x$profileId, character(1)), vapply(profiles, function(x) x$displayName, character(1)))
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
    "ollamaThinkLow",
    "providerProfiles",
    "developerModeEnabled"
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
    "ollamaThinkLow",
    "providerProfiles",
    "developerModeEnabled"
  )
  secretFieldNames = c("anthropicApiKey", "apiKey", "ANTHROPIC_API_KEY")

  kept = config[intersect(names(config), allowedFields)]
  kept = kept[setdiff(names(kept), secretFieldNames)]

  configPath = wmfmConfigPath()
  dir.create(dirname(configPath), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x = kept, path = configPath, auto_unbox = TRUE, pretty = TRUE)

  invisible(configPath)
}


#' Resolve persisted developer-mode preference
#'
#' @return Logical scalar indicating whether developer mode should start
#'   unlocked for this local app session.
#' @keywords internal
resolveDeveloperModePreference = function() {
  localConfig = readWmfmConfig()
  isTRUE(localConfig$developerModeEnabled)
}

#' Persist developer-mode preference locally
#'
#' @param enabled Logical scalar indicating whether developer mode should be
#'   restored as unlocked in later local app sessions.
#'
#' @return Invisibly returns the path that was written.
#' @keywords internal
saveDeveloperModePreference = function(enabled) {
  config = readWmfmConfig()
  config$developerModeEnabled = isTRUE(enabled)
  writeWmfmConfig(config)
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

#' WMFM provider adapter registry
#'
#' Returns deterministic metadata describing supported and future-ready
#' providers, including the adapter construction kind used by WMFM.
#'
#' @return Named list keyed by provider id.
#' @keywords internal
wmfmProviderRegistry = function() {
  list(
    ollama = list(
      provider = "ollama",
      label = "Ollama",
      requiresCredentials = FALSE,
      credentialEnvVar = NULL,
      supportsBaseUrl = TRUE,
      supportsLocalDiscovery = TRUE,
      adapterKind = "chat_ollama",
      requiredConfigFields = c("ollamaBaseUrl", "ollamaModel")
    ),
    claude = list(
      provider = "claude",
      label = "Claude / Anthropic",
      requiresCredentials = TRUE,
      credentialEnvVar = "ANTHROPIC_API_KEY",
      supportsBaseUrl = FALSE,
      supportsLocalDiscovery = FALSE,
      adapterKind = "chat_anthropic",
      requiredConfigFields = character(0)
    ),
    openai = list(
      provider = "openai",
      label = "OpenAI",
      requiresCredentials = TRUE,
      credentialEnvVar = "OPENAI_API_KEY",
      supportsBaseUrl = TRUE,
      supportsLocalDiscovery = FALSE,
      adapterKind = "future",
      requiredConfigFields = character(0)
    ),
    openaiCompatible = list(
      provider = "openaiCompatible",
      label = "OpenAI-compatible",
      requiresCredentials = TRUE,
      credentialEnvVar = "OPENAI_API_KEY",
      supportsBaseUrl = TRUE,
      supportsLocalDiscovery = TRUE,
      adapterKind = "future",
      requiredConfigFields = c("baseUrl")
    )
  )
}

#' List WMFM provider adapters
#'
#' @return Character vector of registry provider ids.
#' @keywords internal
listWmfmProviderAdapters = function() {
  names(wmfmProviderRegistry())
}

#' Test whether a provider is in the WMFM registry
#'
#' @param provider Character scalar provider id.
#'
#' @return Logical scalar.
#' @keywords internal
isWmfmProviderSupported = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  providerId %in% tolower(listWmfmProviderAdapters())
}

#' Get WMFM provider adapter metadata
#'
#' @param provider Character scalar provider id.
#'
#' @return Named list with provider adapter metadata.
#' @keywords internal
getWmfmProviderAdapter = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  registry = wmfmProviderRegistry()
  if (!providerId %in% tolower(names(registry))) {
    supported = paste(sprintf("'%s'", listWmfmProviderAdapters()), collapse = ", ")
    stop(
      paste0(
        "Unsupported chat backend: ", providerId,
        ". Supported backends are ", supported, "."
      ),
      call. = FALSE
    )
  }

  matchedName = names(registry)[tolower(names(registry)) == providerId][1]
  registry[[matchedName]]
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
  registry = wmfmProviderRegistry()

  list(
    ollama = list(
      provider = "ollama",
      requiresCredentials = isTRUE(registry$ollama$requiresCredentials),
      credentialsAvailable = TRUE,
      credentialSource = "none-required",
      localOnly = TRUE
    ),
    claude = list(
      provider = "claude",
      requiresCredentials = isTRUE(registry$claude$requiresCredentials),
      credentialsAvailable = isTRUE(claudeKeyPresent),
      credentialSource = if (isTRUE(claudeKeyPresent)) "env:ANTHROPIC_API_KEY" else "missing",
      localOnly = FALSE
    ),
    openai = list(
      provider = "openai",
      requiresCredentials = isTRUE(registry$openai$requiresCredentials),
      credentialsAvailable = FALSE,
      credentialSource = "not-configured",
      localOnly = FALSE
    ),
    openaiCompatible = list(
      provider = "openaiCompatible",
      requiresCredentials = isTRUE(registry$openaiCompatible$requiresCredentials),
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
