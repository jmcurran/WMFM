#' Build the current chat provider status message
#'
#' @param backend Active chat backend.
#' @param ollamaModel Active Ollama model name.
#' @param ollamaThinkLow Whether Ollama low-thinking mode is active.
#'
#' @return A character string for the chat provider status display.
#' @keywords internal
buildChatProviderStatus = function(backend, ollamaModel, ollamaThinkLow) {
  backend = tolower(trimws(backend %||% "ollama"))

  if (identical(backend, "claude")) {
    return("Current provider: Claude")
  }

  paste0(
    "Current provider: Ollama (model: ",
    ollamaModel %||% "gpt-oss",
    if (isTRUE(ollamaThinkLow)) ", low thinking" else ", normal thinking",
    ")"
  )
}

#' Build the unknown chat provider message
#'
#' @return A character string for an error notification.
#' @keywords internal
buildUnknownChatProviderMessage = function() {
  "Unknown provider selected."
}

#' Build the incorrect Claude provider password message
#'
#' @return A character string for an error notification.
#' @keywords internal
buildClaudeProviderIncorrectPasswordMessage = function() {
  "Incorrect password. Claude was not enabled."
}

#' Build the chat provider set confirmation message
#'
#' @param backend Requested chat backend.
#' @param ollamaModel Active Ollama model name.
#' @param ollamaThinkLow Whether Ollama low-thinking mode is active.
#'
#' @return A character string for a notification.
#' @keywords internal
buildChatProviderSetMessage = function(backend, ollamaModel, ollamaThinkLow) {
  backend = tolower(trimws(backend %||% "ollama"))

  if (identical(backend, "claude")) {
    return("Chat provider set to Claude.")
  }

  paste0(
    "Chat provider set to Ollama using model '",
    ollamaModel %||% "gpt-oss",
    "' with ",
    if (isTRUE(ollamaThinkLow)) "low" else "normal",
    " thinking."
  )
}
