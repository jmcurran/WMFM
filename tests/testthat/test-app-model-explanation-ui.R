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

testthat::test_that("buildExplanationTeachingSummary uses student-facing checklist language", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_response_noun_phrase") = "exam mark"
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"

  audit = buildModelExplanationAudit(model)
  summary = buildExplanationTeachingSummary(audit = audit, model = model)

  testthat::expect_s3_class(summary, "wmfmExplanationTeachingSummary")
  testthat::expect_match(summary$dataDescription, "The explanation starts by orienting the student", fixed = TRUE)
  testthat::expect_match(summary$xChangeDescription, "one predictor goes up by one unit while the other predictors stay fixed", fixed = TRUE)
  testthat::expect_match(summary$mainEffectDescription, "The main result was written", fixed = TRUE)
  testthat::expect_match(summary$researchQuestionLink, "The explanation returns to the research question", fixed = TRUE)

  testthat::expect_identical(
    summary$evidenceTable$section,
    c(
      "Question to answer",
      "Data used",
      "Scale for the result",
      "Starting point",
      "Comparison being described",
      "Uncertainty check"
    )
  )

  testthat::expect_match(
    summary$evidenceTable$summary[[1]],
    "Start by reminding the student what the model is trying to answer:",
    fixed = TRUE
  )
  testthat::expect_match(
    summary$evidenceTable$summary[[3]],
    "Keep the explanation on the original outcome wording",
    fixed = TRUE
  )
})

testthat::test_that("appUI includes explanation and onboarding controls", {
  ui = appUI()
  html = as.character(ui)

  testthat::expect_match(html, "Model Explanation", fixed = TRUE)
  testthat::expect_match(html, "model_explanation", fixed = TRUE)
  testthat::expect_match(html, "Start with the main explanation", fixed = TRUE)
  testthat::expect_match(html, "Load a built-in example", fixed = TRUE)
  testthat::expect_match(html, "loadExampleBtn", fixed = TRUE)
  testthat::expect_match(html, "Research question", fixed = TRUE)
  testthat::expect_no_match(html, "Research question (optional)", fixed = TRUE)
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
  testthat::expect_match(prompt, "- Data used:", fixed = TRUE)
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

getAppServerTextForTest = function() {
  paste(deparse(body(appServer)), collapse = "\n")
}

testthat::test_that("app server keeps fitted model as the post-fit landing tab", {
  serverText = getAppServerTextForTest()

  testthat::expect_match(
    serverText,
    'updateTabsetPanel\\(session, "main_tabs", selected = "Fitted Model"\\)'
  )
  testthat::expect_no_match(serverText, 'selected = "Model Explanation"')
})

testthat::test_that("optional AI tutor sits in the explanation support accordion after the deterministic sections", {
  serverText = getAppServerTextForTest()

  accordionPos = regexpr('model_explanation_support_accordion', serverText, fixed = TRUE)[1]
  claimHeadingPos = regexpr('How each sentence was supported', serverText, fixed = TRUE)[1]
  teachingHeadingPos = regexpr('How to read this explanation', serverText, fixed = TRUE)[1]
  tutorHeadingPos = regexpr('Optional AI tutor', serverText, fixed = TRUE)[1]

  testthat::expect_true(accordionPos > 0)
  testthat::expect_true(claimHeadingPos > 0)
  testthat::expect_true(teachingHeadingPos > 0)
  testthat::expect_true(tutorHeadingPos > 0)
  testthat::expect_lt(claimHeadingPos, teachingHeadingPos)
  testthat::expect_lt(teachingHeadingPos, tutorHeadingPos)
})

testthat::test_that("app server includes load-example support and research-question requirement", {
  serverText = getAppServerTextForTest()

  testthat::expect_match(serverText, 'observeEvent(input$loadExampleBtn,', fixed = TRUE)
  testthat::expect_match(serverText, 'loadExampleSpec(exampleName)', fixed = TRUE)
  testthat::expect_match(
    serverText,
    'Please enter the research question before fitting the model. WMFM uses it to frame the explanation from the start.',
    fixed = TRUE
  )
})


testthat::test_that("renderExplanationTutorUi explains unavailable and available tutor states clearly", {
  unavailableUi = renderExplanationTutorUi(text = NULL, available = FALSE)
  unavailableHtml = as.character(unavailableUi)

  testthat::expect_match(
    unavailableHtml,
    "An AI tutor-style explanation is available only when a chat provider is active.",
    fixed = TRUE
  )

  availableUi = renderExplanationTutorUi(
    text = "Here is a simpler explanation.",
    available = TRUE,
    researchQuestion = "Does Test help explain Exam?",
    dataDescription = "The response variable is `Exam`."
  )
  availableHtml = as.character(availableUi)

  testthat::expect_match(availableHtml, "AI tutor-style explanation", fixed = TRUE)
  testthat::expect_match(availableHtml, "Research question:", fixed = TRUE)
  testthat::expect_match(availableHtml, "Here is a simpler explanation.", fixed = TRUE)
})


testthat::test_that("app server includes explicit fallback text for missing teaching-guide pieces", {
  serverText = getAppServerTextForTest()

  testthat::expect_match(
    serverText,
    'A sentence-by-sentence support map is not available for this explanation yet.',
    fixed = TRUE
  )
  testthat::expect_match(
    serverText,
    'The app could not build the teaching guide for this model yet, so only the main explanation is shown right now.',
    fixed = TRUE
  )
  testthat::expect_match(
    serverText,
    'Turn on a chat provider in Settings if you want the optional AI tutor walkthrough.',
    fixed = TRUE
  )
})
