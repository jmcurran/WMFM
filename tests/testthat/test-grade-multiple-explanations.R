test_that("grade.wmfmModel returns a wmfmGradeListObj for multiple explanations", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = df)

  wm = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    equations = "y = a + bx",
    explanation = "Each one-unit increase in x is associated with an average increase in y."
  )

  g = grade(
    wm,
    explanation = c(
      "Each one-unit increase in x is associated with about a one-point increase in y.",
      "Higher x values are associated with higher y values."
    ),
    autoScore = FALSE
  )

  expect_s3_class(g, "wmfmGradeListObj")
  expect_length(g$grades, 2)
  expect_equal(names(g$grades), c("explanation_1", "explanation_2"))
  expect_true(all(vapply(g$grades, inherits, logical(1), what = "wmfmGrade")))
})


test_that("grade.wmfmModel preserves user-supplied explanation names", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = df)

  wm = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    equations = "y = a + bx",
    explanation = "Each one-unit increase in x is associated with an average increase in y."
  )

  g = grade(
    wm,
    explanation = c(
      answer_b = "Higher x values are associated with higher y values.",
      answer_a = "Each one-unit increase in x is associated with about a one-point increase in y."
    ),
    autoScore = FALSE
  )

  expect_equal(names(g$grades), c("answer_b", "answer_a"))
})
