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
    ollamaBaseUrl = "",
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

#' Resolve a provider profile by identifier
#'
#' @param profileId Character scalar provider profile identifier.
#' @param profiles Optional list of provider profiles.
#'
#' @return A normalised provider profile when available, otherwise `NULL`.
#' @keywords internal
resolveWmfmProviderProfileById = function(profileId, profiles = readWmfmProviderProfiles()) {
  profileId = trimws(as.character(profileId %||% ""))
  if (!nzchar(profileId) || !is.list(profiles) || length(profiles) == 0) {
    return(NULL)
  }

  for (profile in profiles) {
    normalisedProfile = normaliseWmfmProviderProfile(profile)
    if (identical(normalisedProfile$profileId, profileId)) {
      return(normalisedProfile)
    }
  }

  NULL
}

#' Resolve the active WMFM provider profile
#'
#' @param profileId Optional provider profile identifier.
#' @param backend Optional provider type fallback for legacy configuration.
#'
#' @return A normalised provider profile.
#' @keywords internal
resolveWmfmActiveProviderProfile = function(profileId = NULL, backend = NULL) {
  profiles = readWmfmProviderProfiles()
  localConfig = readWmfmConfig()

  requestedProfileId = profileId %||%
    getOption("wmfm.active_provider_profile_id", default = NULL) %||%
    localConfig$activeProviderProfileId %||%
    ""

  matchedProfile = resolveWmfmProviderProfileById(requestedProfileId, profiles)
  if (!is.null(matchedProfile)) {
    return(matchedProfile)
  }

  if (nzchar(requestedProfileId)) {
    requestedProviderType = tolower(trimws(as.character(requestedProfileId)))
    idx = which(vapply(profiles, function(x) {
      profile = normaliseWmfmProviderProfile(x)
      identical(tolower(trimws(as.character(profile$providerType %||% ""))), requestedProviderType)
    }, logical(1)))[1]

    if (!is.na(idx)) {
      return(normaliseWmfmProviderProfile(profiles[[idx]]))
    }
  }

  backend = tolower(trimws(as.character(backend %||% localConfig$backend %||% wmfmProviderDefaults()$backend %||% "")))
  idx = which(vapply(profiles, function(x) {
    profile = normaliseWmfmProviderProfile(x)
    identical(tolower(trimws(as.character(profile$providerType %||% ""))), backend)
  }, logical(1)))[1]

  if (!is.na(idx)) {
    return(normaliseWmfmProviderProfile(profiles[[idx]]))
  }

  normaliseWmfmProviderProfile(profiles[[1]])
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



#' Get the WMFM local configuration directory
#'
#' Returns the directory that WMFM uses for user-specific local configuration.
#' The path respects `options(wmfm.config_dir = ...)` when that option has
#' been set.
#'
#' @return Character scalar path to the WMFM configuration directory.
#' @export
getWmfmConfigDir = function() {
  normalizePath(wmfmConfigDir(), winslash = "/", mustWork = FALSE)
}

#' Get the WMFM local configuration file path
#'
#' Returns the full path to the WMFM user configuration file. This is useful
#' when checking or backing up a local configuration during setup and testing.
#'
#' @return Character scalar path to `config.json`.
#' @export
getWmfmConfigPath = function() {
  normalizePath(wmfmConfigPath(), winslash = "/", mustWork = FALSE)
}

#' Read the WMFM local configuration file path
#'
#' Alias for [getWmfmConfigPath()].
#'
#' @return Character scalar path to `config.json`.
#' @export
readWmfmConfigPath = getWmfmConfigPath

#' Edit the WMFM local configuration file
#'
#' Opens the WMFM user configuration file in the default R editor. If the
#' configuration directory or file does not yet exist, they are created first.
#'
#' @param editor Function used to open the configuration file. Defaults to
#'   [utils::file.edit()]. This argument is mainly provided so tests can avoid
#'   opening an editor.
#'
#' @return Invisibly returns the path to the configuration file.
#' @export
#' @importFrom utils file.edit
editWmfmConfig = function(editor = utils::file.edit) {
  configPath = wmfmConfigPath()
  configDir = dirname(configPath)
  dir.create(configDir, recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(configPath)) {
    jsonlite::write_json(x = list(), path = configPath, auto_unbox = TRUE, pretty = TRUE)
  }

  configPath = normalizePath(configPath, winslash = "/", mustWork = TRUE)
  editor(configPath)
  invisible(configPath)
}

#' Test whether WMFM is running in a deployed app context
#'
#' Uses conservative environment and option checks to distinguish local desktop
#' use from administrator-managed Shiny or CI-style deployments.
#'
#' @return Logical scalar.
#' @keywords internal
isWmfmDeployedApp = function() {
  explicitFlag = tolower(trimws(Sys.getenv("WMFM_DEPLOYED_APP", unset = "")))
  if (explicitFlag %in% c("1", "true", "yes", "on")) {
    return(TRUE)
  }
  if (explicitFlag %in% c("0", "false", "no", "off")) {
    return(FALSE)
  }

  optionFlag = getOption("wmfm.deployed_app", default = NULL)
  if (!is.null(optionFlag)) {
    return(isTRUE(optionFlag))
  }

  any(nzchar(c(
    Sys.getenv("SHINY_PORT", unset = ""),
    Sys.getenv("SHINY_SERVER_VERSION", unset = ""),
    Sys.getenv("RSTUDIO_PRODUCT", unset = "")
  ))) || identical(tolower(trimws(Sys.getenv("CI", unset = ""))), "true")
}

#' Test whether end users may edit provider configuration
#'
#' In local desktop use, provider configuration is editable by default. In a
#' deployed app, configuration is administrator-managed unless an explicit WMFM
#' option or environment flag allows end-user provider selection.
#'
#' @return Logical scalar.
#' @keywords internal
isWmfmProviderConfigurationEditable = function() {
  explicitFlag = tolower(trimws(Sys.getenv("WMFM_ALLOW_USER_PROVIDER_CONFIG", unset = "")))
  if (explicitFlag %in% c("1", "true", "yes", "on")) {
    return(TRUE)
  }
  if (explicitFlag %in% c("0", "false", "no", "off")) {
    return(FALSE)
  }

  optionFlag = getOption("wmfm.allow_user_provider_config", default = NULL)
  if (!is.null(optionFlag)) {
    return(isTRUE(optionFlag))
  }

  !isWmfmDeployedApp()
}

#' Test whether end users may enter provider credentials
#'
#' Credential entry is local-desktop only by default. Deployed apps must receive
#' credentials from the installer through environment variables or deployment
#' secrets rather than from ordinary browser users.
#'
#' @return Logical scalar.
#' @keywords internal
isWmfmCredentialEntryAllowed = function() {
  explicitFlag = tolower(trimws(Sys.getenv("WMFM_ALLOW_USER_CREDENTIAL_ENTRY", unset = "")))
  if (explicitFlag %in% c("1", "true", "yes", "on")) {
    return(!isWmfmDeployedApp())
  }
  if (explicitFlag %in% c("0", "false", "no", "off")) {
    return(FALSE)
  }

  optionFlag = getOption("wmfm.allow_user_credential_entry", default = NULL)
  if (!is.null(optionFlag)) {
    return(isTRUE(optionFlag) && !isWmfmDeployedApp())
  }

  !isWmfmDeployedApp()
}

#' Build provider setup policy text
#'
#' @return Character vector describing who controls provider setup in the
#'   current runtime context.
#' @keywords internal
buildWmfmProviderSetupPolicyText = function() {
  if (isWmfmDeployedApp()) {
    return(c(
      "This appears to be an administrator-managed WMFM deployment.",
      "Ordinary users cannot enter API keys or add provider configuration here.",
      "The installer controls available providers, models, and server credentials."
    ))
  }

  c(
    "This appears to be a local desktop WMFM session.",
    "Provider setup can be managed here without editing hidden files directly.",
    "Credential setup is shown in a separate dialog so API-key guidance is not front and centre."
  )
}

#' Read raw WMFM local configuration
#'
#' Reads the complete local WMFM configuration file. This helper is internal
#' and may include locally stored credential metadata, so user-facing code
#' should prefer `readWmfmConfig()` unless it is explicitly managing secrets.
#'
#' @return Named list containing all parsed configuration fields.
#' @keywords internal
readWmfmRawConfig = function() {
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

  as.list(parsed)
}

#' Read WMFM local configuration
#'
#' Reads persisted local defaults for provider settings. Credential values are
#' deliberately excluded from this ordinary settings view.
#'
#' @return Named list with recognized non-secret fields when present.
#' @keywords internal
readWmfmConfig = function() {
  parsed = readWmfmRawConfig()

  recognizedFields = c(
    "backend",
    "ollamaBaseUrl",
    "ollamaModel",
    "ollamaThinkLow",
    "providerProfiles",
    "activeProviderProfileId",
    "developerModeEnabled"
  )

  parsed[intersect(names(parsed), recognizedFields)]
}

#' Write WMFM local configuration
#'
#' Persists non-secret provider defaults for local reuse while preserving any
#' separately managed local credential block already present in the same WMFM
#' configuration file.
#'
#' @param config Named list with provider defaults to persist.
#'
#' @return Invisibly returns the path that was written.
#' @keywords internal
writeWmfmConfig = function(config = list()) {
  existing = readWmfmRawConfig()
  config = as.list(config)

  allowedFields = c(
    "backend",
    "ollamaBaseUrl",
    "ollamaModel",
    "ollamaThinkLow",
    "providerProfiles",
    "activeProviderProfileId",
    "developerModeEnabled"
  )

  kept = config[intersect(names(config), allowedFields)]
  if ("credentials" %in% names(existing)) {
    kept$credentials = existing$credentials
  }

  configPath = wmfmConfigPath()
  dir.create(dirname(configPath), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x = kept, path = configPath, auto_unbox = TRUE, pretty = TRUE)

  invisible(configPath)
}

#' Test whether local WMFM credential storage is allowed
#'
#' Local credential storage is available only for single-user desktop sessions.
#' Deployed and administrator-managed apps must use environment variables or
#' deployment secrets instead.
#'
#' @return Logical scalar.
#' @keywords internal
isWmfmConfigCredentialStorageAllowed = function() {
  if (isWmfmDeployedApp()) {
    return(FALSE)
  }

  explicitFlag = tolower(trimws(Sys.getenv("WMFM_ALLOW_CONFIG_CREDENTIALS", unset = "")))
  if (explicitFlag %in% c("1", "true", "yes", "on")) {
    return(TRUE)
  }
  if (explicitFlag %in% c("0", "false", "no", "off")) {
    return(FALSE)
  }

  optionFlag = getOption("wmfm.allow_config_credentials", default = NULL)
  if (!is.null(optionFlag)) {
    return(isTRUE(optionFlag))
  }

  TRUE
}

#' Normalise a WMFM provider credential environment variable name
#'
#' @param provider Character scalar provider id.
#'
#' @return Character scalar environment variable name, or an empty string.
#' @keywords internal
normaliseWmfmCredentialEnvVar = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  if (!isWmfmProviderSupported(providerId)) {
    return("")
  }

  getWmfmProviderAdapter(providerId)$credentialEnvVar %||% ""
}

#' Read a locally stored WMFM provider credential
#'
#' @param provider Character scalar provider id.
#'
#' @return Character scalar credential value, or an empty string.
#' @keywords internal
readWmfmConfigCredential = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  if (!nzchar(providerId) || !isWmfmConfigCredentialStorageAllowed()) {
    return("")
  }

  rawConfig = readWmfmRawConfig()
  credentials = rawConfig$credentials
  if (!is.list(credentials) || !providerId %in% tolower(names(credentials))) {
    return("")
  }

  matchedName = names(credentials)[tolower(names(credentials)) == providerId][1]
  credentialEntry = credentials[[matchedName]]
  if (!is.list(credentialEntry)) {
    return("")
  }

  value = credentialEntry$apiKey %||% credentialEntry$key %||% ""
  value = trimws(as.character(value))
  if (length(value) != 1 || is.na(value)) {
    return("")
  }

  value
}

#' Write a locally stored WMFM provider credential
#'
#' @param provider Character scalar provider id.
#' @param credential Character scalar credential value.
#'
#' @return Invisibly returns the config file path written.
#' @keywords internal
writeWmfmConfigCredential = function(provider, credential) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  credential = trimws(as.character(credential %||% ""))

  if (!isWmfmProviderSupported(providerId)) {
    stop("Cannot save credential for an unsupported WMFM provider.", call. = FALSE)
  }
  if (!isWmfmConfigCredentialStorageAllowed()) {
    stop("Local WMFM credential storage is not allowed in this runtime context.", call. = FALSE)
  }
  if (!nzchar(credential)) {
    stop("Credential value cannot be empty.", call. = FALSE)
  }

  rawConfig = readWmfmRawConfig()
  credentials = rawConfig$credentials
  if (!is.list(credentials)) {
    credentials = list()
  }
  credentials[[providerId]] = list(apiKey = credential)
  rawConfig$credentials = credentials

  configPath = wmfmConfigPath()
  dir.create(dirname(configPath), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x = rawConfig, path = configPath, auto_unbox = TRUE, pretty = TRUE)

  invisible(configPath)
}

#' Remove a locally stored WMFM provider credential
#'
#' @param provider Character scalar provider id.
#'
#' @return Invisibly returns the config file path written.
#' @keywords internal
removeWmfmConfigCredential = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  rawConfig = readWmfmRawConfig()
  credentials = rawConfig$credentials
  if (!is.list(credentials)) {
    return(invisible(wmfmConfigPath()))
  }

  keep = setdiff(names(credentials), names(credentials)[tolower(names(credentials)) == providerId])
  rawConfig$credentials = credentials[keep]

  configPath = wmfmConfigPath()
  dir.create(dirname(configPath), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x = rawConfig, path = configPath, auto_unbox = TRUE, pretty = TRUE)

  invisible(configPath)
}

#' Resolve WMFM provider credential availability
#'
#' Environment variables have priority over local config credentials so deployed
#' and scripted use can override desktop settings predictably.
#'
#' @param provider Character scalar provider id.
#'
#' @return Named list with non-secret credential availability metadata.
#' @keywords internal
resolveWmfmProviderCredential = function(provider) {
  providerId = tolower(trimws(as.character(provider %||% "")))
  if (!isWmfmProviderSupported(providerId)) {
    return(list(provider = providerId, available = FALSE, source = "unsupported"))
  }

  adapter = getWmfmProviderAdapter(providerId)
  if (!isTRUE(adapter$requiresCredentials)) {
    return(list(provider = providerId, available = TRUE, source = "none-required"))
  }

  envVar = normaliseWmfmCredentialEnvVar(providerId)
  if (nzchar(envVar) && nzchar(Sys.getenv(envVar, unset = ""))) {
    return(list(provider = providerId, available = TRUE, source = paste0("env:", envVar)))
  }

  if (nzchar(readWmfmConfigCredential(providerId))) {
    return(list(provider = providerId, available = TRUE, source = "wmfm-config"))
  }

  list(provider = providerId, available = FALSE, source = "missing")
}

#' Test whether the developer-mode UI should be exposed
#'
#' Developer mode is intended for maintainers and local debugging. It is hidden
#' unless the local environment explicitly opts in with
#' `WMFM_SHOW_DEVELOPER_MODE=1`.
#'
#' @return Logical scalar.
#' @keywords internal
isDeveloperModeUiEnabled = function() {
  flag = tolower(trimws(Sys.getenv("WMFM_SHOW_DEVELOPER_MODE", unset = "")))
  flag %in% c("1", "true", "yes", "on")
}

#' Resolve developer-mode startup state
#'
#' Developer mode is visible only when `WMFM_SHOW_DEVELOPER_MODE=1` is set
#' locally. When the UI is visible, the enabled/disabled state is restored from
#' the non-secret WMFM config file so development sessions can resume with the
#' same controls enabled.
#'
#' @return Logical scalar.
#' @keywords internal
resolveDeveloperModePreference = function() {
  if (!isDeveloperModeUiEnabled()) {
    return(FALSE)
  }

  config = readWmfmConfig()
  isTRUE(config$developerModeEnabled)
}

#' Persist developer-mode state locally
#'
#' @param enabled Logical scalar.
#'
#' @return Invisibly returns the config file path written by `writeWmfmConfig()`.
#' @keywords internal
saveDeveloperModePreference = function(enabled) {
  config = readWmfmConfig()
  config$developerModeEnabled = isTRUE(enabled)
  writeWmfmConfig(config)
}

#' Test whether a local provider setting was explicitly supplied
#'
#' @return Logical scalar.
#' @keywords internal
hasExplicitWmfmProviderConfig = function() {
  localConfig = readWmfmConfig()
  explicitOption = any(vapply(
    list(
      getOption("wmfm.chat_backend", default = NULL),
      getOption("wmfm.ollama_base_url", default = NULL),
      getOption("wmfm.ollama_model", default = NULL)
    ),
    function(x) {
      !is.null(x) && nzchar(trimws(as.character(x)))
    },
    logical(1)
  ))

  explicitLocal = any(c(
    nzchar(trimws(as.character(localConfig$backend %||% ""))),
    nzchar(trimws(as.character(localConfig$ollamaBaseUrl %||% ""))),
    nzchar(trimws(as.character(localConfig$ollamaModel %||% ""))),
    isTRUE(localConfig$developerModeEnabled),
    is.list(localConfig$providerProfiles) && length(localConfig$providerProfiles) > 0
  ))

  isTRUE(explicitOption) || isTRUE(explicitLocal)
}

#' Test whether a provider is configured well enough for startup
#'
#' @param providerConfig Optional resolved provider configuration.
#'
#' @return Logical scalar.
#' @keywords internal
isWmfmProviderReadyForStartup = function(providerConfig = resolveWmfmProviderConfig()) {
  backend = tolower(trimws(as.character(providerConfig$backend %||% "")))

  if (identical(backend, "claude")) {
    return(hasClaudeApiKey())
  }

  if (identical(backend, "ollama")) {
    return(
      hasExplicitWmfmProviderConfig() &&
        nzchar(trimws(as.character(providerConfig$ollamaBaseUrl %||% ""))) &&
        nzchar(trimws(as.character(providerConfig$ollamaModel %||% "")))
    )
  }

  FALSE
}

#' Build startup guidance for missing provider configuration
#'
#' @return Character scalar.
#' @keywords internal
buildMissingProviderStartupMessage = function() {
  paste(
    "WMFM needs an AI provider before it can generate model explanations.",
    "Set a commercial-provider API key such as ANTHROPIC_API_KEY or OPENAI_API_KEY,",
    "or configure a local Ollama base URL and model.",
    "See the README section on configuring an AI provider before using the app."
  )
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
  activeProfile = resolveWmfmActiveProviderProfile(backend = backend %||% localConfig$backend %||% defaults$backend)

  backendFromOption = getOption("wmfm.chat_backend", default = NULL)
  backendFromProfile = activeProfile$providerType %||% NULL
  backendFromLocal = localConfig$backend %||% NULL
  fallbackBackend = defaults$backend

  resolvedBackend = tolower(trimws(as.character(backend %||%
    backendFromOption %||%
    backendFromProfile %||%
    backendFromLocal %||%
    fallbackBackend)))
  if (!nzchar(resolvedBackend)) {
    resolvedBackend = fallbackBackend
  }

  profileBaseUrl = if (identical(resolvedBackend, "ollama")) activeProfile$apiUrl else NULL
  profileBaseUrl = trimws(as.character(profileBaseUrl %||% ""))
  if (!nzchar(profileBaseUrl)) {
    profileBaseUrl = NULL
  }

  resolvedBaseUrl = trimws(as.character(ollamaBaseUrl %||%
    getOption("wmfm.ollama_base_url", default = NULL) %||%
    profileBaseUrl %||%
    localConfig$ollamaBaseUrl %||%
    defaults$ollamaBaseUrl))
  if (!nzchar(resolvedBaseUrl)) {
    resolvedBaseUrl = defaults$ollamaBaseUrl
  }

  profileModel = trimws(as.character(activeProfile$defaultModel %||% ""))
  if (!nzchar(profileModel)) {
    profileModel = NULL
  }

  resolvedModel = trimws(as.character(ollamaModel %||%
    getOption("wmfm.ollama_model", default = NULL) %||%
    profileModel %||%
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
    ollamaThinkLow = resolvedThinkLow,
    activeProviderProfileId = activeProfile$profileId
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
  isTRUE(resolveWmfmProviderCredential("claude")$available)
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
  registry = wmfmProviderRegistry()
  claudeCredential = resolveWmfmProviderCredential("claude")
  openaiCredential = resolveWmfmProviderCredential("openai")

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
      credentialsAvailable = isTRUE(claudeCredential$available),
      credentialSource = claudeCredential$source,
      localOnly = FALSE
    ),
    openai = list(
      provider = "openai",
      requiresCredentials = isTRUE(registry$openai$requiresCredentials),
      credentialsAvailable = isTRUE(openaiCredential$available),
      credentialSource = openaiCredential$source,
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
    providerConfig = resolveWmfmProviderConfig(backend = details$provider)
    providerReady = isWmfmProviderReadyForStartup(providerConfig)
    list(
      provider = details$provider,
      configured = isTRUE(providerReady),
      requiresCredentials = isTRUE(details$requiresCredentials),
      credentialsAvailable = isTRUE(details$credentialsAvailable),
      credentialSource = details$credentialSource,
      locallyAvailable = isTRUE(details$localOnly),
      ready = isTRUE(providerReady)
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
