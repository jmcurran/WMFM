#' Run the Model Builder app
#'
#' @export
runApp = function() {
  shiny::shinyApp(
    ui = appUI(),
    server = appServer
  )
}
