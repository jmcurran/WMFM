testthat::test_that("Stage 49.3 gives individual-prediction research questions a prediction-first contract", {
  data = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72, 68, 77),
    Test = c(9, 13, 15, 19, 8, 13, 14, 16),
    Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)
  question = "What exam mark would I get if Attend = Yes and Test = 15?"
  attr(model, "wmfm_research_question") = question
  attr(model, "wmfm_research_question_objective") = buildResearchQuestionObjective(model, question)

  block = buildQuestionAwareExplanationPromptBlock(model)

  testthat::expect_match(block, "Primary task: answer the student's individual-prediction research question directly", fixed = TRUE)
  testthat::expect_match(block, "general model summary by itself does not answer", fixed = TRUE)
  testthat::expect_match(block, "individual prediction interval", fixed = TRUE)
})

testthat::test_that("Stage 49.3 prepends the deterministic prediction before general interpretation", {
  data = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72, 68, 77),
    Test = c(9, 13, 15, 19, 8, 13, 14, 16),
    Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)
  question = "What exam mark would I get if Attend = Yes and Test = 15?"
  attr(model, "wmfm_research_question_objective") = buildResearchQuestionObjective(model, question)

  out = prependDeterministicResearchQuestionAnswer(
    explanation = "Attendance and test marks are associated with the exam result.",
    model = model
  )

  testthat::expect_match(out, "WMFM predicts", fixed = TRUE)
  testthat::expect_match(out, "95% prediction interval", fixed = TRUE)
  testthat::expect_lt(
    regexpr("WMFM predicts", out, fixed = TRUE)[[1]],
    regexpr("Attendance and test marks", out, fixed = TRUE)[[1]]
  )
})

testthat::test_that("Stage 49.3 does not turn incomplete profiles into average students", {
  data = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72),
    Test = c(9, 13, 15, 19, 8, 13),
    Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)
  question = "What exam mark would I get for Test = 15?"
  attr(model, "wmfm_research_question_objective") = buildResearchQuestionObjective(model, question)

  block = buildQuestionAwareExplanationPromptBlock(model)
  out = prependDeterministicResearchQuestionAnswer("General explanation.", model)

  testthat::expect_match(block, "Do not silently substitute average", fixed = TRUE)
  testthat::expect_match(out, "Please provide Attend", fixed = TRUE)
  testthat::expect_false(grepl("General explanation", out, fixed = TRUE))
})

testthat::test_that("Stage 49.3 developer Course example is hidden and prediction-first", {
  visibleExamples = listWMFMExamples(includeTestExamples = FALSE)
  developerExamples = listWMFMExamples(includeTestExamples = TRUE)

  testthat::expect_false("test-Course Question-Aware Prediction" %in% visibleExamples)
  testthat::expect_true("test-Course Question-Aware Prediction" %in% developerExamples)

  info = loadExampleSpec("test-Course Question-Aware Prediction")
  testthat::expect_match(info$researchQuestion, "Attend = Yes", fixed = TRUE)
  testthat::expect_match(info$researchQuestion, "Test = 15", fixed = TRUE)
})
