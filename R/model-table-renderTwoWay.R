#' Render a two-way fitted-means table as HTML tags
#'
#' Creates a simple HTML two-way table (as Shiny tag objects) where one factor
#' defines the rows and another defines the columns. Cells are filled from
#' \code{df[[valueName]]} for each \code{rowVar}-\code{colVar} combination.
#'
#' @param df A data frame containing \code{rowVar}, \code{colVar}, and
#'   \code{valueName}.
#' @param rowVar Name of the factor used for table rows.
#' @param colVar Name of the factor used for table columns.
#' @param valueName Name of the numeric fitted-mean column (e.g. \code{".fit"}).
#'
#' @return A Shiny tag object representing an HTML table.
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   df = data.frame(
#'     A = rep(c("a","b"), each = 2),
#'     B = rep(c("u","v"), times = 2),
#'     .fit = c(1, 2, 3, 4)
#'   )
#'   renderTwoWayTable(df, "A", "B", ".fit")
#' }
#'
#' @importFrom shiny tags
#'
#' @export
renderTwoWayTable = function(df, rowVar, colVar, valueName) {
  rowLevels = unique(df[[rowVar]])
  colLevels = unique(df[[colVar]])

  header = shiny::tags$tr(
    shiny::tags$th(rowVar),
    lapply(colLevels, shiny::tags$th)
  )

  bodyRows = lapply(rowLevels, function(r) {
    vals = vapply(colLevels, function(c) {
      w = df[df[[rowVar]] == r & df[[colVar]] == c, valueName]
      if (length(w) == 0) "" else formatC(w[1], digits = 6, format = "fg")
    }, character(1))

    shiny::tags$tr(
      shiny::tags$td(r),
      lapply(vals, shiny::tags$td)
    )
  })

  shiny::tags$table(
    class = "table table-condensed",
    shiny::tags$thead(header),
    shiny::tags$tbody(bodyRows)
  )
}
