testthat::test_that("renderExplanationTutorUi can show research question and data description", {
  ui = renderExplanationTutorUi(
    text = "The app starts with the question and then explains the fitted result.",
    available = TRUE,
    researchQuestion = "Does Test help explain Exam?",
    dataDescription = "The response variable is `Exam`. Number-valued predictors: `Test`."
  )

  html = as.character(ui)

  testthat::expect_match(html, "Research question:", fixed = TRUE)
  testthat::expect_match(html, "Does Test help explain Exam?", fixed = TRUE)
  testthat::expect_match(html, "The response variable is", fixed = TRUE)
})


testthat::test_that("app server guards bucket state updates during example loading", {
  serverText = paste(deparse(body(appServer), width.cutoff = 500L), collapse = "\n")

  testthat::expect_match(
    serverText,
    "setBucketState = function(factors = NULL, continuous = NULL)",
    fixed = TRUE
  )

  testthat::expect_match(
    serverText,
    "observeEvent(input$factors, {",
    fixed = TRUE
  )

  testthat::expect_match(
    serverText,
    "cur = input$factors %||% character(0)",
    fixed = TRUE
  )

  testthat::expect_match(
    serverText,
    "setBucketState(factors = cur, continuous = rv$bucketContinuous %||% character(0))",
    fixed = TRUE
  )

  testthat::expect_match(
    serverText,
    "observeEvent(input$continuous, {",
    fixed = TRUE
  )

  testthat::expect_match(
    serverText,
    "cur = input$continuous %||% character(0)",
    fixed = TRUE
  )

  testthat::expect_match(
    serverText,
    "setBucketState(factors = rv$bucketFactors %||% character(0), continuous = cur)",
    fixed = TRUE
  )
})
