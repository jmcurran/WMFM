test_that("successful profile comparisons override incomplete single-profile predictions", {
  data = data.frame(
    Exam = c(42, 51, 55, 63, 68, 74, 77, 83, 46, 58, 61, 70),
    Attend = factor(
      c("No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
      levels = c("No", "Yes")
    ),
    Test = c(8, 10, 9, 11, 14, 15, 17, 18, 9, 13, 15, 17)
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)

  objective = buildResearchQuestionObjective(
    model,
    "Compare the expected exam marks for attending and non-attending students who both scored 15."
  )

  expect_identical(objective$archetype, "compare_groups_or_profiles")
  expect_identical(objective$answerPayload$status, "ok")
  expect_identical(objective$status, "answerable")
  expect_identical(objective$route, "model_answer")
  expect_false(objective$requiresFollowup)
  expect_length(objective$unsupportedOrMissing, 0L)
  expect_equal(objective$answerPayload$leftProfile$Test, 15)
  expect_equal(objective$answerPayload$rightProfile$Test, 15)
})

test_that("cached general explanations do not displace specialised research-question answers", {
  data = data.frame(
    Exam = c(42, 51, 55, 63, 68, 74, 77, 83, 46, 58, 61, 70),
    Attend = factor(
      c("No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
      levels = c("No", "Yes")
    ),
    Test = c(8, 10, 9, 11, 14, 15, 17, 18, 9, 13, 15, 17)
  )
  model = stats::lm(Exam ~ Attend + Test, data = data)
  question = "Compare the expected exam marks for attending and non-attending students who both scored 15."
  attr(model, "wmfm_research_question") = question
  attr(model, "wmfm_research_question_route") = buildResearchQuestionRoute(model, question)
  attr(model, "wmfm_research_question_objective") = buildResearchQuestionObjective(model, question)

  formulaStr = paste(deparse(stats::formula(model)), collapse = " ")
  coefStr = paste(stats::coef(model), collapse = ";")
  mf = stats::model.frame(model)
  numericAnchorInfo = buildModelNumericAnchorInfo(
    model = model,
    mf = mf,
    predictorNames = names(mf)[-1]
  )
  key = buildLmExplanationCacheKey(
    formulaStr = formulaStr,
    coefStr = coefStr,
    numericAnchorCacheKey = numericAnchorInfo$cacheKey,
    researchQuestion = question,
    followupQuestion = "",
    adjustmentVariables = getModelAdjustmentVariables(model = model)
  )
  oldValue = .env_cache[[key]]
  on.exit({
    if (is.null(oldValue)) {
      .env_cache[[key]] = NULL
    } else {
      .env_cache[[key]] = oldValue
    }
  }, add = TRUE)
  .env_cache[[key]] = "Generic cached model explanation."

  chat = list(chat = function(prompt) stop("Chat should not be called for a cache hit."))
  answer = lmExplanation(model = model, chat = chat, useCache = TRUE)

  expect_match(answer, "second profile is estimated to differ", ignore.case = TRUE)
  expect_false(grepl("Generic cached model explanation", answer, fixed = TRUE))
})
