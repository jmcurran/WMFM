test_that("buildAppExplanationAudit returns the same deterministic audit contract as the model helper", {
  df = getStats20xExamTestData()

  model = stats::lm(Exam ~ Test, data = df)

  appAudit = buildAppExplanationAudit(model)
  modelAudit = buildModelExplanationAudit(model)

  expect_s3_class(appAudit, "wmfmExplanationAudit")
  expect_true(all(names(modelAudit) %in% names(appAudit)))
  expect_identical(appAudit$overview, modelAudit$overview)
  expect_identical(appAudit$promptInputs, modelAudit$promptInputs)
  expect_identical(appAudit$interpretationScale, modelAudit$interpretationScale)
  expect_identical(appAudit$numericAnchor$numericReference, modelAudit$numericAnchor$numericReference)
  expect_identical(appAudit$referenceLevels, modelAudit$referenceLevels)
})

test_that("runModel stores a deterministic explanationAudit on wmfmModel output", {
  testthat::local_mocked_bindings(
    getModelEquations = function(model, method = "deterministic", chat = NULL) {
      "Exam = a + b * Test"
    },
    getChatProvider = function(...) {
      NULL
    },
    .package = "WMFM"
  )

  df = getStats20xExamTestData()[, c("Exam", "Test")]

  out = runModel(
    data = df,
    formula = Exam ~ Test,
    modelType = "lm",
    printOutput = FALSE
  )

  expect_s3_class(out, "wmfmModel")
  expect_s3_class(out$explanationAudit, "wmfmExplanationAudit")
  expect_true(is.data.frame(out$explanationAudit$coefficientTable))
  expect_true(all(c(
    "overview",
    "promptInputs",
    "interpretationScale",
    "numericAnchor",
    "referenceLevels",
    "confidenceIntervals",
    "baselineEvidence",
    "effectEvidence",
    "coefficientTable"
  ) %in% names(out$explanationAudit)))
  expect_identical(out$meta$equationMethod, "deterministic")
  expect_identical(out$meta$equationMethodUsed, "deterministic")
})

test_that("runModel keeps explanationAudit available when chat access is unavailable", {
  testthat::local_mocked_bindings(
    getModelEquations = function(model, method = "deterministic", chat = NULL) {
      "Exam = a + b * Test"
    },
    getChatProvider = function(...) {
      stop("offline test provider")
    },
    .package = "WMFM"
  )

  df = getStats20xExamTestData()[, c("Exam", "Test")]

  out = NULL

  expect_warning(
    out <- runModel(
      data = df,
      formula = Exam ~ Test,
      modelType = "lm",
      printOutput = FALSE
    ),
    regexp = "Could not connect to the language model server"
  )

  expect_s3_class(out, "wmfmModel")
  expect_s3_class(out$explanationAudit, "wmfmExplanationAudit")
  expect_true(is.null(out$explanation) || is.character(out$explanation))
  expect_true(is.null(out$explanationClaimEvidenceMap))
})
