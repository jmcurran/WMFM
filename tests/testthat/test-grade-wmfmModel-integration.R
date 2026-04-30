test_that("grade.wmfmModel can grade the final model explanation by default", {
  df = mtcars
  model = stats::lm(mpg ~ wt, data = df)
  audit = buildModelExplanationAudit(model = model)
  explanation = paste(
    "The fitted model suggests that cars with higher weight tend to have lower mpg.",
    "This is an association in the fitted data rather than proof of causation."
  )
  claimEvidenceMap = buildExplanationClaimEvidenceMap(
    explanationText = explanation,
    audit = audit,
    model = model
  )

  wm = newWmfmModel(
    model = model,
    formula = mpg ~ wt,
    modelType = "lm",
    data = df,
    equations = "mpg = beta0 + beta1 * wt",
    explanation = explanation,
    explanationAudit = audit,
    explanationClaimEvidenceMap = claimEvidenceMap
  )

  gradeObj = grade(wm, autoScore = FALSE)

  expect_s3_class(gradeObj, "wmfmGrade")
  expect_equal(gradeObj$input$explanation, explanation)
  expect_equal(gradeObj$meta$explanationSource, "wmfmModel_final_explanation")
  expect_true(gradeObj$records$student$hasExplanationAudit)
  expect_true(gradeObj$records$student$hasExplanationClaimEvidenceMap)
  expect_equal(
    gradeObj$records$student$scoringContext,
    "final_text_plus_explanation_audit_and_claim_evidence"
  )
  expect_equal(
    gradeObj$records$student$claimEvidenceSentenceCount,
    nrow(claimEvidenceMap$claims)
  )
})


test_that("supplied explanations still override the stored model explanation", {
  df = mtcars
  model = stats::lm(mpg ~ wt, data = df)
  audit = buildModelExplanationAudit(model = model)
  storedExplanation = "The stored final explanation is available on the model."
  suppliedExplanation = "The submitted explanation is graded instead."

  wm = newWmfmModel(
    model = model,
    formula = mpg ~ wt,
    modelType = "lm",
    data = df,
    equations = "mpg = beta0 + beta1 * wt",
    explanation = storedExplanation,
    explanationAudit = audit
  )

  gradeObj = grade(
    wm,
    explanation = suppliedExplanation,
    autoScore = FALSE
  )

  expect_equal(gradeObj$input$explanation, suppliedExplanation)
  expect_equal(gradeObj$meta$explanationSource, "supplied_explanation")
  expect_equal(gradeObj$records$student$explanationText, suppliedExplanation)
  expect_equal(
    gradeObj$records$student$scoringContext,
    "final_text_plus_explanation_audit"
  )
})


test_that("grade.wmfmModel requires an explanation when the model has none", {
  df = mtcars
  model = stats::lm(mpg ~ wt, data = df)

  wm = newWmfmModel(
    model = model,
    formula = mpg ~ wt,
    modelType = "lm",
    data = df,
    equations = "mpg = beta0 + beta1 * wt",
    explanation = NULL
  )

  expect_error(
    grade(wm, autoScore = FALSE),
    "`explanation` must be supplied"
  )
})
