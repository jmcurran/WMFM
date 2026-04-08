test_that("grade.wmfmModel returns a wmfmGrade object", {
  m = makeOfflineWmfmModel()

  g = grade(
    m,
    explanation = paste(
      "x has a positive association with y.",
      "The fitted relationship is descriptive rather than causal."
    )
  )

  expect_s3_class(g, "wmfmGrade")
  expect_true(isTRUE(g$meta$scored))
  expect_true(is.numeric(g$scores$mark))
})


test_that("grade.wmfmModel stores an optional model answer", {
  m = makeOfflineWmfmModel()

  g = grade(
    m,
    explanation = "Higher x tends to be associated with higher y.",
    modelAnswer = paste(
      "Higher x tends to be associated with higher expected y.",
      "This is an association rather than a causal claim."
    )
  )

  expect_true(!is.null(g$records$modelAnswer))
  expect_true(is.data.frame(g$scores$modelAnswer))
})
