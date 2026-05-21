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
