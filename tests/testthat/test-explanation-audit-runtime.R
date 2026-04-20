test_that("buildAppExplanationAudit returns a deterministic audit without chat access", {
  df = getStats20xExamTestData()

  model = stats::lm(Exam ~ Test, data = df)
  out = buildAppExplanationAudit(model)

  expect_s3_class(out, "wmfmExplanationAudit")
  expect_true(is.data.frame(out$baselineEvidence))
})

test_that("runModel stores explanationAudit on wmfmModel output", {
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
})
