test_that("score.wmfmGrade returns structured feedback sections", {
  skip_if_not(exists("grade", mode = "function"))

  m = makeOfflineWmfmModel()

  g = grade(
    m,
    explanation = "Higher x is associated with higher y.",
    modelAnswer = paste(
      "Higher x is associated with higher expected y.",
      "The fitted relationship is positive.",
      "This is an association rather than a causal claim."
    ),
    score = TRUE
  )

  expect_s3_class(g, "wmfmGrade")
  expect_true(is.data.frame(g$feedback$whereMarksLost))
  expect_true(is.data.frame(g$feedback$strengths) || is.null(g$feedback$strengths))
  expect_true(is.data.frame(g$feedback$weaknesses) || is.null(g$feedback$weaknesses))
  expect_true(is.data.frame(g$feedback$missingElements) || is.null(g$feedback$missingElements))
  expect_true(is.data.frame(g$feedback$modelAnswerComparison) || is.null(g$feedback$modelAnswerComparison))
})
