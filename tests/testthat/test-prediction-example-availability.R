testthat::test_that("prediction examples are loadable", {
  exampleNames = c(
    "Course Prediction 1",
    "Course Prediction 2",
    "Course Prediction 3",
    "Oysters Prediction 1",
    "Oysters Prediction 2",
    "Oysters Prediction 3",
    "Quakes Prediction 1",
    "Quakes Prediction 2",
    "Quakes Prediction 3",
    "Diamonds Prediction 1",
    "Diamonds Prediction 2",
    "Diamonds Prediction 3"
  )

  visibleExamples = listWMFMExamples(package = "WMFM")
  testthat::expect_true(all(exampleNames %in% visibleExamples))

  loadedExamples = lapply(exampleNames, loadExampleSpec, package = "WMFM")
  loadedNames = vapply(
    loadedExamples,
    function(exampleInfo) {
      exampleInfo$spec$displayName
    },
    character(1)
  )
  followupQuestions = vapply(
    loadedExamples,
    function(exampleInfo) {
      exampleInfo$followupQuestion
    },
    character(1)
  )

  testthat::expect_identical(loadedNames, exampleNames)
  testthat::expect_true(all(nzchar(followupQuestions)))
  testthat::expect_false(any(grepl("WMFM-", exampleNames, fixed = TRUE)))
})
