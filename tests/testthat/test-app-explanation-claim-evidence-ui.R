testthat::test_that("renderExplanationClaimEvidenceUi returns readable sentence cards", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  claimMap = buildExplanationClaimEvidenceMap(
    explanationText = paste(
      "Does Test help explain Exam?",
      "On average, exam marks tend to increase as Test increases.",
      "These confidence intervals suggest the increase is likely to be positive."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  ui = renderExplanationClaimEvidenceUi(claimMap)
  html = as.character(ui)

  testthat::expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
  testthat::expect_match(html, "Sentence 1", fixed = TRUE)
  testthat::expect_match(html, "This sentence:", fixed = TRUE)
  testthat::expect_match(html, "Evidence used:", fixed = TRUE)
  testthat::expect_match(html, "shows uncertainty in the estimate", fixed = TRUE)
  testthat::expect_false(grepl("deterministic evidence", html, fixed = TRUE))
  testthat::expect_false(grepl("Supporting evidence:", html, fixed = TRUE))
})


testthat::test_that("app server renders the claim-evidence section after the teaching summary", {
  serverText = paste(deparse(body(appServer)), collapse = "\n")

  teachingPos = regexpr('renderExplanationTeachingSummaryUi\\(teachingSummary\\)', serverText, perl = TRUE)[1]
  claimPos = regexpr('renderExplanationClaimEvidenceUi\\(claimMap\\)', serverText, perl = TRUE)[1]
  tutorPos = regexpr('model_explanation_tutor_accordion', serverText, fixed = TRUE)[1]

  testthat::expect_true(teachingPos > 0)
  testthat::expect_true(claimPos > 0)
  testthat::expect_true(tutorPos > 0)
  testthat::expect_lt(teachingPos, claimPos)
  testthat::expect_lt(claimPos, tutorPos)
})
