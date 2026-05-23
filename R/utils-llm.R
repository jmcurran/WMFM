#' Internal helpers and imports for LLM-based model output
#'
#' This file collects shared imports for the internal helper functions that
#' talk to the language model, build prompts, and cache results.
#'
#' @name llmHelpers-internal
#' @keywords internal
#' @importFrom ellmer chat_ollama params type_object type_array type_string
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @importFrom stats model.frame coef terms
NULL


#' Environment for caching LLM results
#'
#' Internal cache for equations and explanations from fitted models.
#'
#' @format An object of class `environment` of length 0.
#' @docType data
#' @keywords internal
.env_cache = new.env(parent = emptyenv())


#' Create a safe chat provider for WMFM
#'
#' Constructs a chat provider used by WMFM, choosing between an Ollama-based
#' provider and an Anthropic Claude-based provider.
#'
#' The backend is selected via the `backend` argument, or if omitted, from the
#' `wmfm.chat_backend` option. When `backend = "ollama"`, the Ollama model can
#' be supplied explicitly via `model`; otherwise `getOption("wmfm.ollama_model")`
#' is used, with a default of `"gpt-oss"`.
#'
#' @param backend Character scalar giving the backend to use. Supported values
#'   are `"ollama"` and `"claude"`.
#' @param model Optional Ollama model name to use when `backend = "ollama"`.
#' @param ollamaThinkLow Logical; when `TRUE`, request low thinking effort
#'   from Ollama models that support the `think` parameter.
#'
#' @return
#' A chat provider object created by `ellmer::chat_ollama()` or
#' `ellmer::chat_anthropic()`, or a dummy provider object (class
#' `"wmfm_dummy_chat_provider"`) that throws a user-friendly error on use.
#'
#' @examples
#' \dontrun{
#'   provider = getChatProvider("ollama")
#'   providerQwen = getChatProvider("ollama", model = "qwen3.5:35b-a3b-bf16")
#'   providerClaude = getChatProvider("claude")
#' }
#'
#' @keywords internal
getChatProvider = function(backend = NULL,
                           model = NULL,
                           ollamaThinkLow = NULL) {

  providerConfig = resolveWmfmProviderConfig(
    backend = backend,
    ollamaModel = model,
    ollamaThinkLow = ollamaThinkLow
  )

  backend = providerConfig$backend

  makeDummyProvider = function(msg, backendName = backend) {
    structure(
      list(
        chat = function(...) {
          stop(msg, call. = FALSE)
        },
        errorMessage = msg,
        backend = backendName
      ),
      class = "wmfm_dummy_chat_provider"
    )
  }

  adapter = tryCatch(
    getWmfmProviderAdapter(backend),
    error = function(err) NULL
  )
  if (is.null(adapter) || identical(adapter$adapterKind, "future")) {
    supported = names(wmfmProviderRegistry())[vapply(
      wmfmProviderRegistry(),
      function(x) !identical(x$adapterKind, "future"),
      logical(1)
    )]
    return(makeDummyProvider(
      paste0(
        "Unsupported chat backend: ", backend,
        ". Supported backends are ", paste(sprintf("'%s'", supported), collapse = ", "), "."
      )
    ))
  }

  constructWmfmChatProvider = function(adapter, providerConfig, makeDummyProvider) {
    backend = adapter$provider

    if (identical(backend, "claude")) {
      if (!hasClaudeApiKey()) {
        return(makeDummyProvider(
          paste(
            "WMFM cannot contact the Claude backend.",
            "",
            "Backend: claude",
            "",
            "ANTHROPIC_API_KEY is not set on this machine.",
            "Set it in the runtime environment before selecting Claude.",
            sep = "\n"
          )
        ))
      }

    claudeModel = getOption("wmfm.claude_model", default = NULL)

    providerArgs = list(
      credentials = function() {
        key = Sys.getenv("ANTHROPIC_API_KEY", unset = "")
        if (!nzchar(key)) {
          stop("ANTHROPIC_API_KEY is not set.")
        }
        key
      }
    )

    if (!is.null(claudeModel) && nzchar(claudeModel)) {
      providerArgs$model = claudeModel
    }

    safeProvider = try(
      do.call(ellmer::chat_anthropic, providerArgs),
      silent = TRUE
    )

    if (inherits(safeProvider, "try-error")) {
      msg = paste(
        "WMFM cannot contact the Claude backend.",
        "",
        "Backend: claude",
        "",
        "This usually means either:",
        "  - the Anthropic credentials are missing or invalid, or",
        "  - the server cannot reach the Anthropic API.",
        "",
        "Check ANTHROPIC_API_KEY and network access, or switch back to Ollama.",
        sep = "\n"
      )
      return(makeDummyProvider(msg))
    }

      return(safeProvider)
    }

  baseUrl = providerConfig$ollamaBaseUrl

  modelName = providerConfig$ollamaModel

  ollamaArgs = list(
    base_url = baseUrl,
    model = modelName
  )

  if (isTRUE(ollamaThinkLow)) {
    ollamaArgs$params = params(think = "low")
  }

  safeProvider = try(
    do.call(chat_ollama, ollamaArgs),
    silent = TRUE
  )

  if (inherits(safeProvider, "try-error")) {
    msg = paste(
      "WMFM cannot contact the Ollama backend.",
      "",
      "Backend: ollama",
      paste0("Server: ", baseUrl),
      paste0("Model: ", modelName),
      "",
      "This usually means either:",
      "  - The Ollama server is not running or unreachable,",
      "  - The configured model name is wrong, or",
      "  - The wmfm.ollama_base_url option is incorrect.",
      "",
      "You can either fix the Ollama server/model settings or switch to Claude.",
      sep = "\n"
    )
    return(makeDummyProvider(msg))
  }

    safeProvider
  }

  constructWmfmChatProvider(
    adapter = adapter,
    providerConfig = providerConfig,
    makeDummyProvider = makeDummyProvider
  )
}

#' Test whether a chat provider is a WMFM dummy provider
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#' @keywords internal
isWmfmDummyChatProvider = function(x) {
  inherits(x, "wmfm_dummy_chat_provider")
}

#' Get a WMFM dummy chat provider message
#'
#' @param x Chat provider object.
#'
#' @return Character scalar with the stored message, or `NULL`.
#' @keywords internal
getWmfmDummyChatProviderMessage = function(x) {
  if (!isWmfmDummyChatProvider(x)) {
    return(NULL)
  }

  msg = x$errorMessage %||% "The selected language model provider is not available."
  msg = trimws(as.character(msg))

  if (!nzchar(msg)) {
    return(NULL)
  }

  msg
}
