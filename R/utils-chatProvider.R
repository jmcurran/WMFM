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
getChatProvider = function(backend = getOption("wmfm.chat_backend", default = "ollama"),
                           model = NULL) {

  backend = tolower(trimws(backend %||% "ollama"))

  makeDummyProvider = function(msg) {
    structure(
      list(
        chat = function(...) {
          stop(msg)
        }
      ),
      class = "wmfm_dummy_chat_provider"
    )
  }

  if (!backend %in% c("ollama", "claude")) {
    return(makeDummyProvider(
      paste0(
        "Unsupported chat backend: ", backend,
        ". Supported backends are 'ollama' and 'claude'."
      )
    ))
  }

  if (identical(backend, "claude")) {
    claudeKey = Sys.getenv("ANTHROPIC_API_KEY", unset = "")

    if (!nzchar(claudeKey)) {
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

  baseUrl = getOption(
    "wmfm.ollama_base_url",
    default = "http://corrin.stat.auckland.ac.nz:11434"
  )

  modelName = model %||% getOption("wmfm.ollama_model", default = "gpt-oss")
  modelName = trimws(modelName %||% "gpt-oss")
  if (!nzchar(modelName)) {
    modelName = "gpt-oss"
  }

  safeProvider = try(
    ellmer::chat_ollama(
      base_url = baseUrl,
      model = modelName
    ),
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
