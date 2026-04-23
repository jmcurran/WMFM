test_that("renderVarSummaryUi returns a Shiny tag object", {

  summaryDf = data.frame(
    var = c("a", "b"),
    class = c("factor", "numeric"),
    missing = c(0L, 1L),
    details = c("Levels: L1, L2", "Range: 1 to 3"),
    stringsAsFactors = FALSE
  )

  ui = renderVarSummaryUi(summaryDf)

  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("renderVarSummaryUi contains expected content", {

  summaryDf = data.frame(
    var = c("a", "b"),
    class = c("factor", "numeric"),
    missing = c(0L, 1L),
    details = c("Levels: L1, L2", "Range: 1 to 3"),
    stringsAsFactors = FALSE
  )

  ui = renderVarSummaryUi(summaryDf)

  html = htmltools::renderTags(ui)$html

  expect_match(html, "Variable")
  expect_match(html, "Class")
  expect_match(html, "Missing")
  expect_match(html, "Details")

  expect_match(html, ">a<")
  expect_match(html, ">factor<")
  expect_match(html, ">1<")
  expect_match(html, "Range: 1 to 3")
})

test_that("renderVarSummaryUi errors when required columns are missing", {

  badDf = data.frame(
    var = "a",
    class = "factor",
    stringsAsFactors = FALSE
  )

  expect_error(renderVarSummaryUi(badDf), "missing required columns")
})
