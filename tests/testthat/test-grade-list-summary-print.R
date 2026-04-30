makeStage153Model = function() {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = df)

  newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    equations = "y = a + bx",
    explanation = "Each one-unit increase in x is associated with an average increase in y."
  )
}

addStage153MethodScore = function(gradeObj, method, mark) {
  block = list(
    mark = mark,
    overallScore = mark * 10,
    metricSummary = data.frame(
      metric = "factualScore",
      label = "Factual accuracy",
      studentValue = mark,
      maxValue = 10,
      stringsAsFactors = FALSE
    )
  )

  gradeObj$scores$byMethod[[method]] = block
  gradeObj$scores$mark = mark
  gradeObj$scores$overallScore = mark * 10
  gradeObj$meta$scored = TRUE
  gradeObj$meta$scoredMethods = unique(c(gradeObj$meta$scoredMethods, method))
  gradeObj$meta$lastScoredMethod = method
  gradeObj
}

test_that("summary.wmfmGradeListObj reports method-specific scoring structure", {
  wm = makeStage153Model()

  g = grade(
    wm,
    explanation = c(
      explanation_01 = "The fitted line goes up as x increases.",
      explanation_02 = "The slope is positive, so larger x values predict larger y values."
    ),
    autoScore = FALSE
  )

  g$grades[[1]] = addStage153MethodScore(g$grades[[1]], "deterministic", 7)
  g$grades[[2]] = addStage153MethodScore(g$grades[[2]], "deterministic", 9)
  g$grades[[2]] = addStage153MethodScore(g$grades[[2]], "llm", 8)
  g$meta$scored = TRUE
  g$meta$scoredMethods = c("deterministic", "llm")
  g$meta$lastScoredMethod = "llm"

  s = summary(g)

  expect_s3_class(s, "summary.wmfmGradeListObj")
  expect_equal(s$scoredByMethod$deterministic$n, 2)
  expect_true(s$scoredByMethod$deterministic$complete)
  expect_equal(s$scoredByMethod$llm$n, 1)
  expect_false(s$scoredByMethod$llm$complete)
  expect_equal(s$deterministicMark$mean, 8)
  expect_equal(s$llmMark$mean, 8)
  expect_equal(s$markByExplanation$lastMethod, c("deterministic", "llm"))
  expect_equal(s$markByExplanation$latestMark, c(7, 8))
})

test_that("print.wmfmGradeListObj displays latest method-specific marks", {
  wm = makeStage153Model()

  g = grade(
    wm,
    explanation = c(
      explanation_01 = "The fitted line goes up as x increases.",
      explanation_02 = "The slope is positive, so larger x values predict larger y values."
    ),
    autoScore = FALSE
  )

  g$grades[[1]] = addStage153MethodScore(g$grades[[1]], "deterministic", 6)
  g$grades[[2]] = addStage153MethodScore(g$grades[[2]], "deterministic", 7)
  g$grades[[2]] = addStage153MethodScore(g$grades[[2]], "llm", 8)
  g$meta$scoredMethods = c("deterministic", "llm")
  g$meta$lastScoredMethod = "llm"

  printed = utils::capture.output(print(g))

  expect_true(any(grepl("explanation_01: 6.00 / 10", printed, fixed = TRUE)))
  expect_true(any(grepl("(deterministic)", printed, fixed = TRUE)))
  expect_true(any(grepl("explanation_02: 8.00 / 10", printed, fixed = TRUE)))
  expect_true(any(grepl("(llm)", printed, fixed = TRUE)))
})

test_that("print.summary.wmfmGradeListObj includes scored method counts", {
  wm = makeStage153Model()

  g = grade(
    wm,
    explanation = c(
      explanation_01 = "The fitted line goes up as x increases.",
      explanation_02 = "The slope is positive, so larger x values predict larger y values."
    ),
    autoScore = FALSE
  )

  g$grades[[1]] = addStage153MethodScore(g$grades[[1]], "deterministic", 6)
  g$grades[[2]] = addStage153MethodScore(g$grades[[2]], "deterministic", 8)
  s = summary(g)

  printed = utils::capture.output(print(s))

  expect_true(any(grepl("Deterministic scored explanations: 2 / 2", printed, fixed = TRUE)))
  expect_true(any(grepl("LLM scored explanations: 0 / 2", printed, fixed = TRUE)))
  expect_true(any(grepl("Latest mark mean: 7.00", printed, fixed = TRUE)))
})
