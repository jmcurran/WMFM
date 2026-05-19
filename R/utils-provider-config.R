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

  resolvedBackend = tolower(trimws(backend %||% getOption("wmfm.chat_backend", default = defaults$backend)))
  if (!nzchar(resolvedBackend)) {
    resolvedBackend = defaults$backend
  }

  resolvedBaseUrl = trimws(as.character(ollamaBaseUrl %||% getOption("wmfm.ollama_base_url", default = defaults$ollamaBaseUrl)))
  if (!nzchar(resolvedBaseUrl)) {
    resolvedBaseUrl = defaults$ollamaBaseUrl
  }

  resolvedModel = trimws(as.character(ollamaModel %||% getOption("wmfm.ollama_model", default = defaults$ollamaModel)))
  if (!nzchar(resolvedModel)) {
    resolvedModel = defaults$ollamaModel
  }

  resolvedThinkLow = isTRUE(ollamaThinkLow %||% getOption("wmfm.ollama_think_low", default = defaults$ollamaThinkLow))

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
