#' Run the Model Builder app
#'
#' @export
runWMFMApp = function() {
  shiny::shinyApp(
    ui = appUI(),
    server = appServer
  )
}
