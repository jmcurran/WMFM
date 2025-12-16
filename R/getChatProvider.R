#' Create a safe chat provider for WMFM
#'
#' Constructs a chat provider used by WMFM, choosing between an Ollama-based
#' provider and a Google Gemini-based provider.
#'
#' The backend is selected via the \code{wmfm.chat_backend} option:
#' \itemize{
#'   \item \code{"ollama"} (default): use \code{ellmer::chat_ollama()} with
#'         the base URL taken from the \code{wmfm.ollama_base_url} option.
#'   \item \code{"gemini"}: use \code{ellmer::chat_google_gemini()} to connect
#'         to the latest default Gemini model exposed by \pkg{ellmer}.
#' }
#'
#' If the chosen backend cannot be constructed (e.g. Ollama server down,
#' missing Gemini credentials, or network issues), the function returns a
#' dummy provider whose \code{chat()} method always throws a clear,
#' user-friendly error message. This prevents the Shiny app from crashing
#' when the language model backend is unavailable.
#'
#' @details
#' The Ollama base URL is read from:
#' \preformatted{
#'   getOption("wmfm.ollama_base_url",
#'             default = "http://corrin.stat.auckland.ac.nz:11434")
#' }
#'
#' Backend selection is controlled via:
#' \preformatted{
#'   options(wmfm.chat_backend = "ollama")  # default
#'   options(wmfm.chat_backend = "gemini")  # use Google Gemini
#' }
#'
#' Authentication and low-level configuration for Google Gemini are handled
#' entirely by \code{ellmer::chat_google_gemini()}.
#'
#' The dummy provider returned on failure has class
#' \code{"wmfm_dummy_chat_provider"} and a single method, \code{chat()},
#' which always raises an informative error.
#'
#' @return
#' A chat provider object created by \code{ellmer::chat_ollama()} or
#' \code{ellmer::chat_google_gemini()}, depending on the configured backend,
#' or a dummy provider object (class \code{"wmfm_dummy_chat_provider"}) that
#' throws a user-friendly error on use.
#'
#' @examples
#' \dontrun{
#'   ## Default: Ollama
#'   provider <- getChatProvider()
#'
#'   ## Switch to Gemini
#'   options(wmfm.chat_backend = "gemini")
#'   provider_gemini <- getChatProvider()
#' }
#'
#' @keywords internal
getChatProvider = function() {

  backend = getOption("wmfm.chat_backend", default = "ollama")

  # Helper: construct a dummy provider with a clear message
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

  # ------------------------
  # Gemini backend
  # ------------------------
  if (identical(backend, "gemini")) {
    safeProvider = try(
      ellmer::chat_google_gemini(),
      silent = TRUE
    )

    if (inherits(safeProvider, "try-error")) {
      msg = paste(
        "\\u274C WMFM cannot contact the Google Gemini backend.",
        "",
        "Backend: gemini",
        "",
        "This usually means either:",
        "  \\u2022 The Gemini configuration (e.g. API key or credentials) is missing or invalid, or",
        "  \\u2022 The server cannot reach the Gemini API endpoint.",
        "",
        "You can either fix the Gemini configuration or switch back to Ollama, e.g.:",
        '  options(wmfm.chat_backend = "ollama")',
        sep = "\n"
      )
      return(makeDummyProvider(msg))
    }

    return(safeProvider)
  }

  # ------------------------
  # Ollama backend (default)
  # ------------------------
  baseUrl = getOption(
    "wmfm.ollama_base_url",
    default = "http://corrin.stat.auckland.ac.nz:11434"
  )

  safeProvider = try(
    ellmer::chat_ollama(
      base_url = baseUrl,
      model    = "gpt-oss"
    ),
    silent = TRUE
  )

  if (inherits(safeProvider, "try-error")) {
    msg = paste(
      "\\u274CWMFM cannot contact the Ollama backend.",
      "",
      "Backend: ollama",
      paste0("Server: ", baseUrl),
      "",
      "This usually means either:",
      "  \\u2022 The Ollama server is not running or unreachable, or",
      "  \\u2022 The wmfm.ollama_base_url option is incorrect.",
      "",
      "You can either fix the Ollama server/URL or switch to Gemini, e.g.:",
      '  options(wmfm.chat_backend = "gemini")',
      sep = "\n"
    )
    return(makeDummyProvider(msg))
  }

  safeProvider
}
