testthat::test_that("cleanExplanationText removes leading Answer formatting artifacts", {
  testthat::expect_identical(
    cleanExplanationText("Answer Therefore, on average, students improve."),
    "Therefore, on average, students improve."
  )

  testthat::expect_identical(
    cleanExplanationText("Answer: On average, exam marks increase."),
    "On average, exam marks increase."
  )

  testthat::expect_identical(
    cleanExplanationText("ANSWER - On average, expected counts decrease."),
    "On average, expected counts decrease."
  )

  testthat::expect_identical(
    cleanExplanationText("answer On average, the fitted value is higher."),
    "On average, the fitted value is higher."
  )
})


testthat::test_that("cleanExplanationText preserves ordinary explanation content", {
  text = "On average, exam marks increase by 2.5 points. The 95% confidence interval is from 1.1 to 3.9."

  testthat::expect_identical(cleanExplanationText(text), text)
  testthat::expect_identical(cleanExplanationText(NA_character_), NA_character_)
  testthat::expect_null(cleanExplanationText(NULL))
})


testthat::test_that("cleanExplanationText removes Answer artifacts after sentence boundaries", {
  text = paste(
    "The study asks whether Test helps explain Exam.",
    "Answer: On average, exam marks increase as Test increases."
  )

  testthat::expect_identical(
    cleanExplanationText(text),
    paste(
      "The study asks whether Test helps explain Exam.",
      "On average, exam marks increase as Test increases."
    )
  )
})


testthat::test_that("buildExplanationClaimEvidenceMap uses cleaned text for sentence units", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  out = buildExplanationClaimEvidenceMap(
    explanationText = paste(
      "Does Test help explain Exam?",
      "Answer: On average, exam marks tend to increase as Test increases."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_equal(nrow(out$claims), 2)
  testthat::expect_identical(
    out$claims$claimText[[2]],
    "On average, exam marks tend to increase as Test increases."
  )
  testthat::expect_identical(out$claims$claimTags[[2]], c("effect", "answer"))
})


testthat::test_that("cleanExplanationText removes markdown-like Answer headings", {
  testthat::expect_identical(
    cleanExplanationText("**Answer:** On average, exam marks increase."),
    "On average, exam marks increase."
  )

  testthat::expect_identical(
    cleanExplanationText("## Answer\nOn average, exam marks increase."),
    "On average, exam marks increase."
  )

  testthat::expect_identical(
    cleanExplanationText("The study asks whether Test helps explain Exam.\n**Answer:** On average, exam marks increase."),
    "The study asks whether Test helps explain Exam.\nOn average, exam marks increase."
  )

  testthat::expect_identical(
    cleanExplanationText("Answer to the research question: On average, exam marks increase."),
    "On average, exam marks increase."
  )
})
