test_that("student explanation toolbar uses compact statistical buttons", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  html = as.character(buildStudentExplanationToolbarUi(model))

  expect_match(html, "openStudentCoefficientDialog", fixed = TRUE)
  expect_match(html, "openStudentMeanDialog", fixed = TRUE)
  expect_match(html, "openStudentDifferenceDialog", fixed = TRUE)
  expect_match(html, "openStudentResidualDialog", fixed = TRUE)
  expect_match(html, "openStudentOtherDialog", fixed = TRUE)
  expect_match(html, "wmfm-statistical-insert-button", fixed = TRUE)
})

test_that("logistic insertion dialogs offer appropriate scales", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  context = studentExplanationModelContext(model)

  expect_named(context$coefficientScales, c("Log-odds coefficient", "Odds ratio"))
  expect_named(context$meanScales, c("Probability", "Odds", "Log odds"))
  expect_named(context$differenceScales, c("Probability difference", "Odds ratio", "Log-odds difference"))
})

test_that("Poisson insertion dialogs offer appropriate scales", {
  model = stats::glm(cyl ~ wt, data = mtcars, family = stats::poisson())
  context = studentExplanationModelContext(model)

  expect_true("Expected count" %in% names(context$meanScales))
  expect_true("Expected-count ratio" %in% names(context$differenceScales))
})

test_that("coefficient fragments can include transformed intervals", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  fragment = studentExplanationCoefficientFragment(model, "wt", "ratio", TRUE, 0.95)

  expect_match(fragment, "odds ratio", fixed = TRUE)
  expect_match(fragment, "95% confidence interval", fixed = TRUE)
})

test_that("fitted response fragments use requested scales", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())

  probability = studentExplanationMeanFragment(model, 1L, "response", TRUE, 0.95)
  odds = studentExplanationMeanFragment(model, 1L, "odds", FALSE, 0.95)

  expect_match(probability, "predicted probability", fixed = TRUE)
  expect_match(odds, "fitted odds", fixed = TRUE)
})

test_that("pairwise comparisons support odds ratios", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  fragment = studentExplanationDifferenceFragment(model, 1L, 2L, "ratio", TRUE, 0.95)

  expect_match(fragment, "odds ratio", fixed = TRUE)
  expect_match(fragment, "observation 2", fixed = TRUE)
})

test_that("residual and other-statistic fragments are available", {
  model = stats::lm(mpg ~ wt, data = mtcars)

  residual = studentExplanationResidualFragment(model, 1L, "response")
  rSquared = studentExplanationOtherFragment(model, "rSquared")

  expect_match(residual, "residual for observation 1", fixed = TRUE)
  expect_match(rSquared, "R-squared", fixed = TRUE)
})

test_that("student explanation UI retains editor and feedback controls", {
  html = as.character(appUI())

  expect_match(html, "Write an explanation", fixed = TRUE)
  expect_match(html, "studentExplanationText", fixed = TRUE)
  expect_match(html, "checkStudentExplanation", fixed = TRUE)
  expect_match(html, "wmfmInsertStudentExplanation", fixed = TRUE)
})

test_that("prediction toolbar button is available", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  html = as.character(buildStudentExplanationToolbarUi(model))

  expect_match(html, "openStudentPredictionDialog", fixed = TRUE)
  expect_match(html, "Insert an individual prediction", fixed = TRUE)
})

test_that("linear predictions can include prediction intervals", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  fragment = studentExplanationPredictionFragment(
    model,
    mtcars[1, "wt", drop = FALSE],
    "response",
    TRUE,
    0.95
  )

  expect_match(fragment, "predicted individual value", fixed = TRUE)
  expect_match(fragment, "95% prediction interval", fixed = TRUE)
})

test_that("linear predictions can use typical-value wording", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  fragment = studentExplanationPredictionFragment(
    model,
    mtcars[1, "wt", drop = FALSE],
    "response",
    FALSE,
    0.95,
    wording = "typical"
  )

  expect_match(fragment, "typical value", fixed = TRUE)
  expect_false(grepl("prediction interval", fragment, fixed = TRUE))
})

test_that("logistic predictions offer probability and odds without intervals", {
  model = stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  context = studentExplanationModelContext(model)
  probability = studentExplanationPredictionFragment(
    model,
    mtcars[1, "wt", drop = FALSE],
    "response",
    TRUE,
    0.95
  )
  odds = studentExplanationPredictionFragment(
    model,
    mtcars[1, "wt", drop = FALSE],
    "odds",
    TRUE,
    0.95
  )

  expect_named(
    context$predictionScales,
    c("Predicted probability", "Predicted odds", "Predicted log odds")
  )
  expect_match(probability, "predicted probability", fixed = TRUE)
  expect_match(odds, "predicted odds", fixed = TRUE)
  expect_false(grepl("prediction interval", probability, fixed = TRUE))
})

test_that("Poisson predictions are clearly separated from expected means", {
  model = stats::glm(cyl ~ wt, data = mtcars, family = stats::poisson())
  fragment = studentExplanationPredictionFragment(
    model,
    mtcars[1, "wt", drop = FALSE],
    "response",
    TRUE,
    0.95
  )

  expect_match(fragment, "predicted count", fixed = TRUE)
  expect_false(grepl("prediction interval", fragment, fixed = TRUE))
})

test_that("prediction inputs support controlled broadcasting", {
  data = data.frame(
    outcome = c(10, 12, 16, 18),
    Attend = factor(c("Yes", "No", "Yes", "No")),
    Test = c(5, 10, 15, 20)
  )
  model = stats::lm(outcome ~ Attend + Test, data = data)
  parsed = parseStudentExplanationPredictionValues(
    model,
    list(
      studentPredictionValue1 = "Yes",
      studentPredictionValue2 = "5, 10, 15"
    )
  )

  expect_true(parsed$ok)
  expect_equal(parsed$predictionCount, 3L)
  expect_equal(as.character(parsed$newData$Attend), rep("Yes", 3))
  expect_equal(parsed$newData$Test, c(5, 10, 15))
})

test_that("prediction inputs reject incompatible non-singleton lengths", {
  data = data.frame(
    outcome = c(10, 12, 16, 18),
    Attend = factor(c("Yes", "No", "Yes", "No")),
    Test = c(5, 10, 15, 20)
  )
  model = stats::lm(outcome ~ Attend + Test, data = data)
  parsed = parseStudentExplanationPredictionValues(
    model,
    list(
      studentPredictionValue1 = c("Yes", "No"),
      studentPredictionValue2 = "5, 10, 15"
    )
  )

  expect_false(parsed$ok)
  expect_match(parsed$message, "either one value or 3 values", fixed = TRUE)
})

test_that("prediction fragments describe supplied covariate profiles", {
  data = data.frame(
    outcome = c(10, 12, 16, 18),
    Attend = factor(c("Yes", "No", "Yes", "No")),
    Test = c(5, 10, 15, 20)
  )
  model = stats::lm(outcome ~ Attend + Test, data = data)
  newData = data.frame(
    Attend = factor(rep("Yes", 3), levels = levels(data$Attend)),
    Test = c(5, 10, 15)
  )
  fragment = studentExplanationPredictionFragment(
    model,
    newData,
    "response",
    TRUE,
    0.95
  )

  expect_match(fragment, "Attend = Yes, Test = 5", fixed = TRUE)
  expect_match(fragment, "Attend = Yes, Test = 10", fixed = TRUE)
  expect_match(fragment, "Attend = Yes, Test = 15", fixed = TRUE)
  expect_equal(length(strsplit(fragment, "; ", fixed = TRUE)[[1]]), 3L)
})

test_that("prediction dialog uses covariate controls rather than observations", {
  data = data.frame(
    outcome = c(10, 12, 16, 18),
    Attend = factor(c("Yes", "No", "Yes", "No")),
    Test = c(5, 10, 15, 20)
  )
  model = stats::lm(outcome ~ Attend + Test, data = data)
  html = as.character(buildStudentExplanationPredictionDialog(model, 0.95))

  expect_match(html, "studentPredictionValue1", fixed = TRUE)
  expect_match(html, "studentPredictionValue2", fixed = TRUE)
  expect_match(html, "studentPredictionPreviewUi", fixed = TRUE)
  expect_false(grepl("studentPredictionObservation", html, fixed = TRUE))
})

test_that("fitted means accept user-defined covariate profiles", {
  model = stats::lm(mpg ~ am + wt, data = mtcars)
  parsed = parseStudentExplanationPredictionValues(
    model,
    list(
      studentMeanValue1 = c("0"),
      studentMeanValue2 = "2.5, 3.0, 3.5"
    ),
    inputPrefix = "studentMeanValue"
  )

  expect_true(parsed$ok)
  expect_equal(nrow(parsed$newData), 3L)
  fragment = studentExplanationMeanFragment(model, parsed$newData, "response", TRUE, 0.95)
  expect_match(fragment, "expected value", fixed = TRUE)
  expect_match(fragment, "95% confidence interval", fixed = TRUE)
  expect_match(fragment, "wt = 3.5", fixed = TRUE)
})

test_that("fitted-mean dialog uses direct covariate entry", {
  model = stats::lm(mpg ~ am + wt, data = mtcars)
  html = as.character(buildStudentExplanationMeanDialog(model, 0.95))

  expect_match(html, "studentMeanValue1", fixed = TRUE)
  expect_match(html, "studentMeanValue2", fixed = TRUE)
  expect_match(html, "studentMeanPreviewUi", fixed = TRUE)
  expect_false(grepl("studentMeanObservation", html, fixed = TRUE))
})

test_that("developer diagnostic report contains grading evidence", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  gradeObj = list(scores = list(overallScore = 40), feedback = list(message = "example"))
  report = buildStudentExplanationDiagnosticReport(
    model,
    rv = NULL,
    input = NULL,
    explanationText = "The predicted value is 20.",
    gradeObj = gradeObj
  )

  expect_match(report, "WMFM student explanation diagnostic report", fixed = TRUE)
  expect_match(report, "mpg ~ wt", fixed = TRUE)
  expect_match(report, "The predicted value is 20.", fixed = TRUE)
  expect_match(report, "overallScore", fixed = TRUE)
})

test_that("student explanation UI includes developer diagnostic output", {
  html = as.character(appUI())
  expect_match(html, "studentExplanationDeveloperDiagnosticsUi", fixed = TRUE)
})
