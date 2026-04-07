# These are starter tests for the new grading workflow.
# They assume the runModel/wmfmModel refactor has already been applied.

test_that("grade.wmfmModel returns a wmfmGrade object", {
  skip_if_not(exists("runModel", mode = "function"))

  m = runModel(
    data = mtcars,
    formula = mpg ~ wt,
    modelType = "lm",
    printOutput = FALSE
  )

  g = grade(
    m,
    explanation = paste(
      "Cars with greater weight tend to have lower expected miles per gallon.",
      "The fitted relationship is negative and the model is descriptive rather than causal."
    )
  )

  expect_s3_class(g, "wmfmGrade")
  expect_true(isTRUE(g$meta$scored))
  expect_true(is.numeric(g$scores$mark))
})


test_that("grade.wmfmModel stores an optional model answer", {
  skip_if_not(exists("runModel", mode = "function"))

  m = runModel(
    data = mtcars,
    formula = mpg ~ wt,
    modelType = "lm",
    printOutput = FALSE
  )

  g = grade(
    m,
    explanation = "Heavier cars tend to have lower mpg.",
    modelAnswer = "Heavier cars tend to have lower expected mpg, and this is an association rather than a causal claim."
  )

  expect_true(!is.null(g$records$modelAnswer))
  expect_true(is.data.frame(g$scores$modelAnswer))
})
