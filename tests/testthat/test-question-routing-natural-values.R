makeStage498CourseModel = function() {
  data = data.frame(
    Exam = c(42, 55, 61, 68, 73, 80, 48, 59),
    Attend = factor(c("No", "Yes", "Yes", "No", "Yes", "Yes", "No", "No"), levels = c("No", "Yes")),
    Test = c(8, 10, 12, 14, 16, 18, 9, 15)
  )
  stats::lm(Exam ~ Attend + Test, data = data)
}

testthat::test_that("Stage 49.8 extracts ordinary course profile wording", {
  model = makeStage498CourseModel()

  missingAttendance = buildResearchQuestionObjective(
    model,
    "I got 17 in the test. How will I do in the exam?"
  )
  testthat::expect_equal(missingAttendance$profile$Test, 17)
  testthat::expect_setequal(missingAttendance$unsupportedOrMissing, "Attend")

  expectedResponse = buildResearchQuestionObjective(
    model,
    "What is the expected exam mark for students who attend and score 15 in the test?"
  )
  testthat::expect_identical(expectedResponse$archetype, "expected_response")
  testthat::expect_identical(expectedResponse$profile$Attend, "Yes")
  testthat::expect_equal(expectedResponse$profile$Test, 15)
  testthat::expect_false(expectedResponse$requiresFollowup)
})

testthat::test_that("Stage 49.8 records explicit outcome thresholds", {
  model = makeStage498CourseModel()
  objective = buildResearchQuestionObjective(
    model,
    "Will I get over 50 in the exam if I attended regularly and got 15 in the test?"
  )

  testthat::expect_equal(objective$outcomeThreshold, 50)
  testthat::expect_identical(objective$profile$Attend, "Yes")
  testthat::expect_equal(objective$profile$Test, 15)
})

testthat::test_that("Stage 49.8 constructs natural binary comparison profiles", {
  model = makeStage498CourseModel()
  objective = buildResearchQuestionObjective(
    model,
    "Compare the expected exam marks for attending and non-attending students who both scored 15."
  )

  testthat::expect_identical(objective$archetype, "compare_groups_or_profiles")
  testthat::expect_identical(objective$answerPayload$status, "ok")
  testthat::expect_identical(objective$answerPayload$leftProfile$Attend, "Yes")
  testthat::expect_identical(objective$answerPayload$rightProfile$Attend, "No")
  testthat::expect_equal(objective$answerPayload$leftProfile$Test, 15)
  testthat::expect_equal(objective$answerPayload$rightProfile$Test, 15)
})

testthat::test_that("Stage 49.8 recognises adjusted-effect wording", {
  model = makeStage498CourseModel()
  objective = buildResearchQuestionObjective(
    model,
    "Does attendance matter after allowing for the test mark?"
  )

  testthat::expect_identical(objective$archetype, "estimate_or_interpret_effect")
})
