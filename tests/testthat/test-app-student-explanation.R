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
    1L,
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
    1L,
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
    1L,
    "response",
    TRUE,
    0.95
  )
  odds = studentExplanationPredictionFragment(
    model,
    1L,
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
    1L,
    "response",
    TRUE,
    0.95
  )

  expect_match(fragment, "predicted count", fixed = TRUE)
  expect_false(grepl("prediction interval", fragment, fixed = TRUE))
})
