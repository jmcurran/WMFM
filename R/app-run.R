#' Run the Model Builder app
#'
#' Launches the WMFM Shiny application. The Ollama base URL used for
#' language-model calls can be configured here.
#'
#' @param ollamaBaseUrl Optional character string giving the base URL of
#'   the Ollama server, for example \code{"http://localhost:11434"}.
#'   If \code{NULL}, the function uses the current value of the option
#'   \code{"wmfm.ollama_base_url"} if set, otherwise a built-in default.
#'
#' @return A \code{shiny.appobj}, invisibly.
#'
#' @export
runWMFMApp = function(ollamaBaseUrl = NULL) {
  if (!is.null(ollamaBaseUrl)) {
    options(wmfm.ollama_base_url = ollamaBaseUrl)
  }

  shiny::shinyApp(
    ui     = appUI(),
    server = appServer
  )
}
