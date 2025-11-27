#' Get the chat provider used by the app
#'
#' Returns a chat provider object configured to talk to a local Ollama
#' server. The function is written so that switching to another provider
#' (e.g. OpenAI) later is straightforward.
#'
#' @return An object representing a chat provider, suitable for use with
#'   \code{ellmer}.
#' @keywords internal
getChatProvider = function() {
  # If you later want OpenAI, you can uncomment this:
  # if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  #   return(chat_openai())
  # }

  # For now: always use Ollama
  chat_ollama(
    base_url = "http://corrin.stat.auckland.ac.nz:11434",
    model = "gpt-oss"
  )
}
