test_that("score.wmfmGrade returns structured feedback sections", {
  skip_if_not(exists("runModel", mode = "function"))
  skip_if_not(exists("grade", mode = "function"))

  m = runModel(
    data = mtcars,
    formula = mpg ~ wt,
    modelType = "lm",
    printOutput = FALSE
  )

  g = grade(
    m,
    explanation = "Heavier cars tend to have lower mpg.",
    modelAnswer = paste(
      "Heavier cars tend to have lower expected miles per gallon.",
      "The fitted relationship is negative.",
      "This is an association rather than a causal claim."
    )
  )

  expect_s3_class(g, "wmfmGrade")
  expect_true(is.data.frame(g$feedback$whereMarksLost))
  expect_true(is.data.frame(g$feedback$strengths) || is.null(g$feedback$strengths))
  expect_true(is.data.frame(g$feedback$weaknesses) || is.null(g$feedback$weaknesses))
  expect_true(is.data.frame(g$feedback$missingElements) || is.null(g$feedback$missingElements))
  expect_true(is.data.frame(g$feedback$modelAnswerComparison) || is.null(g$feedback$modelAnswerComparison))
})
