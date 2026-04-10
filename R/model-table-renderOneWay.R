#' Render a one-way fitted-means table as HTML tags
#'
#' Creates a simple HTML table (as Shiny tag objects) for a one-factor fitted
#' means display.
#'
#' @param df A data frame containing the factor column and a fitted mean column.
#' @param rowVar The name of the factor column used for rows.
#' @param valueName The name of the numeric fitted-mean column (e.g. \code{".fit"}).
#'
#' @return A Shiny tag object representing an HTML table.
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   df = data.frame(A = c("a","b"), .fit = c(1.2, 2.3))
#'   renderOneWayTable(df, "A", ".fit")
#' }
#'
#' @importFrom shiny tags
#'
#' @export
renderOneWayTable = function(df, rowVar, valueName) {
  rows = lapply(seq_len(nrow(df)), function(i) {
    shiny::tags$tr(
      shiny::tags$td(df[[rowVar]][i]),
      shiny::tags$td(formatC(df[[valueName]][i], digits = 6, format = "fg"))
    )
  })

  shiny::tags$table(
    class = "table table-condensed",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th(rowVar),
        shiny::tags$th("Fitted mean")
      )
    ),
    shiny::tags$tbody(rows)
  )
}
