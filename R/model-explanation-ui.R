#' Map explanation zoom selection to CSS font size
#'
#' @param zoomLevel Character scalar zoom level from the UI control.
#'
#' @return A character scalar containing a CSS font-size value.
#' @keywords internal
mapExplanationZoomToFontSize = function(zoomLevel) {
  if (!is.character(zoomLevel) || length(zoomLevel) != 1 || is.na(zoomLevel)) {
    return("1rem")
  }

  zoomMap = c(
    small = "0.95rem",
    normal = "1.08rem",
    large = "1.28rem",
    presentation = "1.6rem"
  )

  unname(zoomMap[zoomLevel] %||% "1rem")
}

#' Render explanation text as safe paragraph HTML
#'
#' @param text Character scalar explanation text.
#' @param zoomLevel Character scalar zoom level from the UI control.
#'
#' @return A shiny tag object containing safe paragraph HTML.
#' @keywords internal
#' @importFrom htmltools htmlEscape
#' @importFrom shiny HTML tags
renderSafeExplanationHtml = function(text, zoomLevel = "normal") {
  if (is.null(text) || !is.character(text) || length(text) != 1) {
    return(NULL)
  }

  cleanText = trimws(text)
  if (!nzchar(cleanText)) {
    return(NULL)
  }

  paragraphs = strsplit(cleanText, "\n\\s*\n", perl = TRUE)[[1]]
  paragraphs = trimws(paragraphs)
  paragraphs = paragraphs[nzchar(paragraphs)]

  if (length(paragraphs) == 0) {
    return(NULL)
  }

  paragraphTags = lapply(paragraphs, function(paragraphText) {
    safeText = htmlEscape(paragraphText, attribute = TRUE)
    safeText = gsub("\n", "<br/>", safeText, fixed = TRUE)
    tags$p(HTML(safeText))
  })

  tags$div(
    class = "wmfm-explanation-body",
    style = paste0("font-size: ", mapExplanationZoomToFontSize(zoomLevel), ";"),
    paragraphTags
  )
}
