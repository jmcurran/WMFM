testthat::test_that("Stage 18.2 scoring and grading examples are packaged", {
  exampleNames = listWMFMExamples(includeTestExamples = TRUE)

  expectedNames = paste0("test-SG-", 1:6)

  testthat::expect_true(all(expectedNames %in% exampleNames))
})

testthat::test_that("Stage 18.2 scoring and grading example specs load", {
  expectedFormulas = c(
    "Exam ~ Test",
    "Exam ~ Assignment",
    "Exam ~ Gender",
    "Pass ~ StudyHours",
    "Exam ~ Attend * Gender + Test",
    "Errors ~ PracticeHours"
  )

  expectedModelTypes = c(
    "lm",
    "lm",
    "lm",
    "logistic",
    "lm",
    "poisson"
  )

  for (i in seq_along(expectedFormulas)) {
    exampleName = paste0("test-SG-", i)
    exampleInfo = loadExampleSpec(exampleName)

    testthat::expect_identical(exampleInfo$spec$formula, expectedFormulas[[i]])
    testthat::expect_identical(exampleInfo$spec$modelType, expectedModelTypes[[i]])
    testthat::expect_s3_class(exampleInfo$data, "data.frame")
    testthat::expect_true(nrow(exampleInfo$data) > 0)
    testthat::expect_true(!is.null(exampleInfo$researchQuestion))
  }
})
