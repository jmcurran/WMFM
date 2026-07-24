test_that("student explanation choices contain coefficients and intervals", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  choices = buildStudentExplanationInsertionChoices(model)

  expect_named(choices, c("coefficients", "intervals"))
  expect_true(any(grepl("estimated coefficient", choices$coefficients, fixed = TRUE)))
  expect_true(any(grepl("95% confidence interval", choices$intervals, fixed = TRUE)))
  expect_true(any(grepl("wt", choices$coefficients, fixed = TRUE)))
})

test_that("student explanation choices are empty without a model", {
  choices = buildStudentExplanationInsertionChoices(NULL)

  expect_identical(choices$coefficients, character(0))
  expect_identical(choices$intervals, character(0))
})

test_that("student explanation term formatting is readable", {
  expect_identical(
    formatStudentExplanationTerm(c("(Intercept)", "groupB", "x:groupB")),
    c("the intercept", "groupB", "x by groupB")
  )
})

test_that("student explanation UI includes the editor and toolbar", {
  html = as.character(appUI())

  expect_match(html, "Write an explanation", fixed = TRUE)
  expect_match(html, "studentExplanationText", fixed = TRUE)
  expect_match(html, "studentExplanationToolbarUi", fixed = TRUE)
  expect_match(html, "wmfmInsertStudentExplanation", fixed = TRUE)
})

test_that("student explanation feedback exposes strengths and revision priorities", {
  model = makeOfflineWmfmModel()
  gradeObj = grade(
    model,
    explanation = "Higher x is associated with higher y.",
    method = "deterministic",
    autoScore = TRUE
  )

  feedback = buildStudentExplanationFeedback(gradeObj)

  expect_type(feedback, "list")
  expect_true(is.finite(feedback$overallScore))
  expect_type(feedback$strengths, "character")
  expect_type(feedback$priorities, "character")

  html = as.character(renderStudentExplanationFeedbackUi(feedback))
  expect_match(html, "Feedback on your explanation", fixed = TRUE)
  expect_match(html, "What is working well", fixed = TRUE)
  expect_match(html, "What to revise next", fixed = TRUE)
})

test_that("student explanation UI includes formative checking controls", {
  html = as.character(appUI())

  expect_match(html, "checkStudentExplanation", fixed = TRUE)
  expect_match(html, "Check my explanation", fixed = TRUE)
  expect_match(html, "studentExplanationFeedbackStatus", fixed = TRUE)
  expect_match(html, "studentExplanationFeedbackUi", fixed = TRUE)
})
