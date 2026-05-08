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

testthat::test_that("Stage 18.8 SG examples use s20x course.df", {
  expectedVariables = list(
    `test-SG-1` = c("Exam", "Test"),
    `test-SG-2` = c("Exam", "Assignment", "Assign"),
    `test-SG-3` = c("Exam", "Gender"),
    `test-SG-4` = c("Pass", "StudyHours", "Test"),
    `test-SG-5` = c("Exam", "Attend", "Gender", "Test")
  )

  for (exampleName in names(expectedVariables)) {
    exampleInfo = loadExampleSpec(exampleName)

    testthat::expect_identical(exampleInfo$spec$dataSource, "package")
    testthat::expect_identical(exampleInfo$spec$dataPackage, "s20x")
    testthat::expect_identical(exampleInfo$spec$dataObject, "course.df")
    testthat::expect_identical(exampleInfo$spec$dataTransform, "courseDfScoringGradingAliases")
    testthat::expect_s3_class(exampleInfo$data, "data.frame")
    testthat::expect_true(all(expectedVariables[[exampleName]] %in% names(exampleInfo$data)))
  }
})

testthat::test_that("Stage 18.8 SG-5 interaction is estimable from s20x course.df", {
  exampleInfo = loadExampleSpec("test-SG-5")
  model = stats::lm(stats::as.formula(exampleInfo$spec$formula), data = exampleInfo$data)
  coefficients = stats::coef(model)
  interactionRows = grepl(":", names(coefficients), fixed = TRUE)

  testthat::expect_true(any(interactionRows))
  testthat::expect_false(any(is.na(coefficients[interactionRows])))
  testthat::expect_equal(qr(stats::model.matrix(model))$rank, ncol(stats::model.matrix(model)))
})
