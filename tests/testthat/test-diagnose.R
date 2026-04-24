test_that("diagnose returns wmfmMetricDiagnosis object", {
  scores = makeMockScores(
    detValues = c(0, 0, 0, 0, 0),
    llmValues = c(2, 2, 2, 2, 2)
  )

  runs = makeMockRuns(
    explanations = c("a", "b", "c", "d", "e")
  )

  dx = diagnose(scores, metric = "numericExpressionAdequate", runs = runs)

  expect_s3_class(dx, "wmfmMetricDiagnosis")
  expect_equal(dx$metric, "numericExpressionAdequate")
  expect_true(is.data.frame(dx$summary))
  expect_true(is.data.frame(dx$runDiagnosis))
  expect_true(is.data.frame(dx$flaggedRuns))
})

test_that("diagnose identifies fully systematic upward disagreement", {
  scores = makeMockScores(
    detValues = c(0, 0, 0, 0, 0),
    llmValues = c(2, 2, 2, 2, 2)
  )

  runs = makeMockRuns(
    explanations = c("a", "b", "c", "d", "e")
  )

  dx = diagnose(scores, metric = "numericExpressionAdequate", runs = runs)
  s = dx$summary[1, , drop = FALSE]

  expect_equal(s$disagreementRate, 1)
  expect_true(s$detConstant)
  expect_true(s$llmConstant)
  expect_equal(s$meanLlmMinusDet, 2)
  expect_equal(s$diagnosisClass, "deterministicRuleLikelyTooStrict")
})

test_that("diagnose identifies no disagreement", {
  scores = makeMockScores(
    detValues = c(2, 2, 1, 1),
    llmValues = c(2, 2, 1, 1)
  )

  runs = makeMockRuns(
    explanations = c("a", "b", "c", "d")
  )

  dx = diagnose(scores, metric = "numericExpressionAdequate", runs = runs)
  s = dx$summary[1, , drop = FALSE]

  expect_equal(s$disagreementRate, 0)
  expect_equal(s$diagnosisClass, "noSystematicDisagreement")
  expect_equal(nrow(dx$flaggedRuns), 0)
})

test_that("diagnose attaches run context when runs is supplied", {
  scores = makeMockScores(
    detValues = c(0, 0),
    llmValues = c(2, 2)
  )

  runs = makeMockRuns(
    explanations = c("increase by 2", "roughly two points higher"),
    effectScaleClaim = c("not_stated", "additive"),
    percentLanguageMention = c(FALSE, FALSE)
  )

  dx = diagnose(scores, metric = "numericExpressionAdequate", runs = runs)

  expect_true("explanationText" %in% names(dx$runDiagnosis))
  expect_true("effectScaleClaim" %in% names(dx$runDiagnosis))
  expect_equal(dx$runDiagnosis$effectScaleClaim, c("not_stated", "additive"))
})

test_that("diagnose warns when runs is omitted", {
  scores = makeMockScores(
    detValues = c(0, 0),
    llmValues = c(2, 2)
  )

  expect_warning(
    dx <- diagnose(scores, metric = "numericExpressionAdequate"),
    "No `runs` object supplied"
  )

  expect_s3_class(dx, "wmfmMetricDiagnosis")
})

test_that("diagnose errors on invalid metric", {
  scores = makeMockScores(
    detValues = c(0, 0),
    llmValues = c(2, 2)
  )

  expect_error(
    diagnose(scores, metric = NA_character_),
    "metric must be NULL or a single"
  )
})
test_that("diagnose.default errors for unsupported object class", {
  x = list()

  expect_error(
    diagnose(x, metric = "numericExpressionAdequate"),
    "No diagnose\\(\\) method"
  )
})
