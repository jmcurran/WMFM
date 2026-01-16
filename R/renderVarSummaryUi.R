#' Render a variable summary table as Shiny UI
#'
#' Converts a variable-summary data frame (as produced by \code{buildVarSummary()})
#' into a simple HTML table for display in a Shiny UI.
#'
#' @param summaryDf A data frame with (at minimum) columns \code{var}, \code{class},
#'   \code{missing}, and \code{details}.
#'
#' @return A Shiny tag object (a \code{shiny.tag.list}) containing a table.
#'
#' @importFrom shiny tags tagList
#'
#' @examples
#' summaryDf = data.frame(
#'   var = c("a", "b"),
#'   class = c("factor", "numeric"),
#'   missing = c(0L, 1L),
#'   details = c("Levels: L1, L2", "Range: 1 to 3"),
#'   stringsAsFactors = FALSE
#' )
#'
#' ui = WMFM:::renderVarSummaryUi(summaryDf)
#' ui
#'
#' @keywords internal
renderVarSummaryUi = function(summaryDf) {

  if (!is.data.frame(summaryDf)) {
    stop("summaryDf must be a data frame.")
  }

  required = c("var", "class", "missing", "details")
  missingCols = setdiff(required, names(summaryDf))
  if (length(missingCols) > 0) {
    stop(
      paste0(
        "summaryDf is missing required columns: ",
        paste(missingCols, collapse = ", ")
      )
    )
  }

  shiny::tagList(
    shiny::tags$table(
      class = "table table-sm table-striped",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Variable"),
          shiny::tags$th("Class"),
          shiny::tags$th("Missing"),
          shiny::tags$th("Details")
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(nrow(summaryDf)), function(i) {
          shiny::tags$tr(
            shiny::tags$td(summaryDf$var[i]),
            shiny::tags$td(summaryDf$class[i]),
            shiny::tags$td(as.character(summaryDf$missing[i])),
            shiny::tags$td(summaryDf$details[i])
          )
        })
      )
    )
  )
}
