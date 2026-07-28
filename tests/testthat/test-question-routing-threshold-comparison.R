test_that("pass wording is treated as a missing outcome threshold", {
  data = data.frame(
    Exam = c(43, 51, 58, 64, 67, 74, 79, 86),
    Attend = factor(
      c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
      levels = c("No", "Yes")
    ),
    Test = c(8, 9, 11, 12, 14, 15, 17, 18)
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)

  objective = buildResearchQuestionObjective(
    model,
    "Will I pass if I attended regularly and got 12 in the test?"
  )

  expect_identical(objective$route, "needs_input")
  expect_true("outcome_threshold" %in% objective$unsupportedOrMissing)
  expect_identical(objective$profile$Attend, "Yes")
  expect_equal(objective$profile$Test, 12)
})

test_that("natural attendance comparisons build two complete profiles", {
  data = data.frame(
    Exam = c(43, 51, 58, 64, 67, 74, 79, 86),
    Attend = factor(
      c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
      levels = c("No", "Yes")
    ),
    Test = c(8, 9, 11, 12, 14, 15, 17, 18)
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)

  payload = computeResearchQuestionProfileComparison(
    model,
    "Compare the expected exam marks for attending and non-attending students who both scored 15."
  )

  expect_identical(payload$status, "ok")
  expect_identical(payload$leftProfile$Attend, "Yes")
  expect_identical(payload$rightProfile$Attend, "No")
  expect_equal(payload$leftProfile$Test, 15)
  expect_equal(payload$rightProfile$Test, 15)
})

test_that("specialised answers do not append a generic explanation", {
  data = data.frame(
    Exam = c(43, 51, 58, 64, 67, 74, 79, 86),
    Attend = factor(
      c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
      levels = c("No", "Yes")
    ),
    Test = c(8, 9, 11, 12, 14, 15, 17, 18)
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)
  objective = buildResearchQuestionObjective(
    model,
    "What exam mark should I expect if I attended regularly and got 16 in the test?"
  )
  attr(model, "wmfm_research_question_objective") = objective

  answer = prependDeterministicResearchQuestionAnswer("Generic explanation.", model)

  expect_match(answer, "WMFM predicts", fixed = TRUE)
  expect_false(grepl("Generic explanation", answer, fixed = TRUE))
})
