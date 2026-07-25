test_that("student explanation choices contain coefficients and intervals", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  choices = buildStudentExplanationInsertionChoices(model)

  expect_named(choices, c("coefficients", "intervals", "effects", "predictions"))
  expect_true(any(grepl("estimated response-scale coefficient", choices$coefficients, fixed = TRUE)))
  expect_true(any(grepl("95% confidence interval", choices$intervals, fixed = TRUE)))
  expect_true(any(grepl("wt", choices$coefficients, fixed = TRUE)))
})

test_that("student explanation choices are empty without a model", {
  choices = buildStudentExplanationInsertionChoices(NULL)

  expect_identical(choices$coefficients, character(0))
  expect_identical(choices$intervals, character(0))
  expect_identical(choices$effects, character(0))
  expect_identical(choices$predictions, character(0))
})

test_that("student explanation term formatting is readable", {
  expect_identical(
    formatStudentExplanationTerm(c("(Intercept)", "groupB", "x:groupB")),
    c("the intercept", "groupB", "x interacting with groupB")
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

test_that("student explanation choices are model-family aware", {
  logisticModel = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  logisticChoices = buildStudentExplanationInsertionChoices(logisticModel)

  expect_true(any(grepl("log-odds coefficient", logisticChoices$coefficients, fixed = TRUE)))
  expect_true(any(grepl("odds ratio", logisticChoices$effects, fixed = TRUE)))
  expect_true(any(grepl("predicted probability", logisticChoices$predictions, fixed = TRUE)))

  poissonModel = stats::glm(cyl ~ wt, data = mtcars, family = stats::poisson())
  poissonChoices = buildStudentExplanationInsertionChoices(poissonModel)

  expect_true(any(grepl("log-count coefficient", poissonChoices$coefficients, fixed = TRUE)))
  expect_true(any(grepl("expected-count ratio", poissonChoices$effects, fixed = TRUE)))
  expect_true(any(grepl("expected count", poissonChoices$predictions, fixed = TRUE)))
})

test_that("student explanation terms describe transformations and interactions", {
  formatted = formatStudentExplanationTerm(c(
    "log(x)",
    "sqrt(z)",
    "I(age^2)",
    "x:groupB"
  ))

  expect_identical(
    formatted,
    c(
      "log-transformed x",
      "square-root transformed z",
      "age squared",
      "x interacting with groupB"
    )
  )
})

test_that("student explanation toolbar exposes effects and fitted results for GLMs", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  html = as.character(buildStudentExplanationToolbarUi(model))

  expect_match(html, "studentExplanationEffect", fixed = TRUE)
  expect_match(html, "Insert odds ratio", fixed = TRUE)
  expect_match(html, "studentExplanationPrediction", fixed = TRUE)
  expect_match(html, "Predicted Probability", fixed = TRUE)
})
