testthat::test_that("renderExplanationTeachingSummaryUi puts main pieces first", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_response_noun_phrase") = "exam mark"
  audit = buildModelExplanationAudit(model)
  summary = buildExplanationTeachingSummary(audit = audit, model = model)
  ui = renderExplanationTeachingSummaryUi(summary)
  html = as.character(ui)

  firstPos = regexpr("Main pieces of information used", html, fixed = TRUE)[1]
  secondPos = regexpr("Scale used for the explanation", html, fixed = TRUE)[1]

  testthat::expect_true(firstPos > 0)
  testthat::expect_true(secondPos > 0)
  testthat::expect_lt(firstPos, secondPos)
})

testthat::test_that("appUI includes a dedicated Model Explanation tab", {
  ui = appUI()
  html = as.character(ui)

  testthat::expect_match(html, "Model Explanation", fixed = TRUE)
  testthat::expect_match(html, "model_explanation", fixed = TRUE)
})

testthat::test_that("buildExplanationTutorPrompt is constrained by the teaching summary", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"
  audit = buildModelExplanationAudit(model)
  summary = buildExplanationTeachingSummary(audit = audit, model = model)

  prompt = buildExplanationTutorPrompt(
    teachingSummary = summary,
    modelExplanation = "Higher test marks are associated with higher exam marks.",
    researchQuestion = "Does Test help explain Exam?"
  )

  testthat::expect_match(prompt, "Use only the information provided below.", fixed = TRUE)
  testthat::expect_match(prompt, "Do not invent new calculations", fixed = TRUE)
  testthat::expect_match(prompt, "Main pieces of information used", fixed = TRUE)
  testthat::expect_match(prompt, "Research question: Does Test help explain Exam\\?", perl = TRUE)
})

testthat::test_that("buildAppTeachingTutorExplanation uses a chat provider when available", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_response_noun_phrase") = "exam mark"
  audit = buildModelExplanationAudit(model)
  summary = buildExplanationTeachingSummary(audit = audit, model = model)

  fakeChatProvider = list(
    chat = function(prompt) {
      testthat::expect_match(prompt, "friendly statistics tutor", fixed = TRUE)
      "This is a simpler tutor-style explanation."
    }
  )

  out = buildAppTeachingTutorExplanation(
    teachingSummary = summary,
    chatProvider = fakeChatProvider,
    modelExplanation = "Some explanation text."
  )

  testthat::expect_identical(out, "This is a simpler tutor-style explanation.")
})
