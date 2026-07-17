#' Register the reproducible-analysis download handler
#'
#' @param output Shiny output object.
#' @param rv App reactive values object.
#'
#' @return No return value; called for observer side effects.
#'
#' @keywords internal
#'
#' @importFrom shiny downloadHandler req
registerAnalysisDownloadObserver = function(output, rv) {
  output$analysisDownload = downloadHandler(
    filename = function() {
      recipe = req(rv$analysisRecipe)
      paste0("wmfm_analysis.", analysisRecipeDownloadExtension(recipe))
    },
    content = function(file) {
      recipe = req(rv$analysisRecipe)
      writeAnalysisRecipeDownload(
        recipe = recipe,
        analysisData = rv$data,
        path = file
      )
    },
    contentType = "application/octet-stream"
  )

  invisible(NULL)
}
