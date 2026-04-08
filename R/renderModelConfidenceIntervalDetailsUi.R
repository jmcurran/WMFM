#' Render confidence-interval derivation details as UI
#'
#' Converts the derivation records produced by
#' \code{buildModelConfidenceIntervalData()} into a compact accordion so that
#' students can inspect how a displayed confidence interval is constructed
#' without overwhelming the main table.
#'
#' @param details A list of derivation records.
#'
#' @return A Shiny UI object.
#' @keywords internal
#'
#' @importFrom bslib accordion accordion_panel
#' @importFrom htmltools HTML tagList tags
renderModelConfidenceIntervalDetailsUi = function(details) {

  if (length(details) == 0) {
    return(NULL)
  }

  panels = lapply(details, function(oneDetail) {
    accordion_panel(
      title = oneDetail$label,
      tagList(
        tags$p(tags$strong("Context"), tags$br(), oneDetail$context),
        tags$p(tags$strong("Linear combination"), tags$br(), HTML(oneDetail$combination)),
        tags$p(tags$strong("Variance rule"), tags$br(), HTML(oneDetail$variance)),
        tags$p(tags$strong("Why this matters"), tags$br(), oneDetail$note)
      )
    )
  })

  do.call(
    accordion,
    c(
      list(
        id = "model_confint_details",
        multiple = TRUE,
        open = FALSE
      ),
      panels
    )
  )
}
