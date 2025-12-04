#' Get the chat provider used by WMFM
#'
#' Chooses between OpenAI (if configured) and Ollama. The Ollama base URL
#' can be configured via the \code{wmfm.ollama_base_url} option, which is
#' set by \code{runApp(ollamaBaseUrl = ...)}.
#'
#' @return A chat provider object from the \pkg{ellmer} package.
#' @keywords internal
#'
#' @importFrom ellmer chat_ollama
getChatProvider = function() {

  ## If you later want to support OpenAI directly, you can still do:
  ## if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  ##   return(ellmer::chat_openai())
  ## }

  baseUrl = getOption(
    "wmfm.ollama_base_url",
    default = "http://corrin.stat.auckland.ac.nz:11434"
  )

  ellmer::chat_ollama(
    base_url = baseUrl,
    model = "gpt-oss"
  )
}
