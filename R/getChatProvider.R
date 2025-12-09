#' Create a safe chat provider for WMFM
#'
#' Returns an Ollama-based chat provider (from the \pkg{ellmer} package)
#' using the base URL configured via the \code{wmfm.ollama_base_url} option.
#' If the provider cannot be constructed (e.g., the Ollama server is
#' unreachable or misconfigured), the function returns a dummy chat provider
#' that consistently throws a clear, user-friendly error message whenever its
#' \code{chat()} method is called. This prevents the Shiny app from crashing
#' and allows it to fail gracefully when the language model is unavailable.
#'
#' The default URL is:
#' \preformatted{
#' http://corrin.stat.auckland.ac.nz:11434
#' }
#'
#' @details
#' The dummy provider returned on failure has class
#' \code{"wmfm_dummy_chat_provider"} and contains a single method,
#' \code{chat()}, which always raises an informative error. This ensures that
#' downstream code that attempts to call the LLM receives a clear diagnostic
#' message instead of an unexpected exception.
#'
#' @return
#' A chat provider object created by \code{ellmer::chat_ollama()}, or a dummy
#' provider object that throws a user-friendly error on use.
#'
#' @examples
#' \dontrun{
#'   provider <- getChatProvider()
#'   provider$chat("Hello!")   # Works if the Ollama server is available
#' }
#'
#' @seealso
#' \code{\link[ellmer]{chat_ollama}} for the underlying provider constructor.
#'
#' @keywords internal
getChatProvider = function() {

  baseUrl = getOption(
    "wmfm.ollama_base_url",
    default = "http://corrin.stat.auckland.ac.nz:11434"
  )

  safeProvider <- try(
    ellmer::chat_ollama(
      base_url = baseUrl,
      model = "gpt-oss"
    ),
    silent = TRUE
  )

  if (inherits(safeProvider, "try-error")) {
    # Return a dummy provider that always errors nicely
    structure(
      list(
        chat = function(...) {
          stop(
            "❌ WMFM cannot contact the configured LLM.\n",
            "Server: ", baseUrl, "\n\n",
            "Check your connection or change the Ollama URL."
          )
        }
      ),
      class = "wmfm_dummy_chat_provider"
    )
  } else {
    safeProvider
  }
}
