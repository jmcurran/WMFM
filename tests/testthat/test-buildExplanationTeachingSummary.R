testthat::test_that("buildExplanationTeachingSummary returns a stable student-facing structure", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_dataset_doc") = "Exam: exam mark\nTest: test mark"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  out = buildExplanationTeachingSummary(audit = audit, model = model)

  testthat::expect_s3_class(out, "wmfmExplanationTeachingSummary")
  testthat::expect_named(
    out,
    c(
      "dataDescription",
      "interpretationScale",
      "baselineChoice",
      "xChangeDescription",
      "mainEffectDescription",
      "uncertaintySummary",
      "evidenceTable",
      "researchQuestionLink"
    )
  )
  testthat::expect_true(all(vapply(out[1:6], is.character, logical(1))))
  testthat::expect_true(all(vapply(out[1:6], function(x) length(x) == 1 && !is.na(x) && nzchar(x), logical(1))))
  testthat::expect_true(is.data.frame(out$evidenceTable))
  testthat::expect_true(nrow(out$evidenceTable) >= 5)
  testthat::expect_true(all(c("section", "summary") %in% names(out$evidenceTable)))
  testthat::expect_match(out$dataDescription, "The response variable is `Exam`")
  testthat::expect_match(out$dataDescription, "Number-valued predictors: `Test`")
  testthat::expect_identical(out$evidenceTable$section[[1]], "Research question")
  testthat::expect_identical(out$evidenceTable$section[[2]], "Data used")
  testthat::expect_match(out$baselineChoice, "sample mean|pretending every variable begins at zero")
  testthat::expect_match(out$baselineChoice, "11\\.57")
  testthat::expect_match(out$baselineChoice, "Zero lies outside the observed range")
  testthat::expect_match(out$xChangeDescription, "marks")
  testthat::expect_match(out$researchQuestionLink, "Does Test help explain Exam\\?")
})

testthat::test_that("buildExplanationTeachingSummary fills all fields when no research question is supplied", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_response_noun_phrase") = "exam mark"
  audit = buildModelExplanationAudit(model)
  out = buildExplanationTeachingSummary(audit = audit, model = model, researchQuestion = NULL)

  testthat::expect_true(all(!vapply(out[c(
    "dataDescription",
    "interpretationScale",
    "baselineChoice",
    "xChangeDescription",
    "mainEffectDescription",
    "uncertaintySummary",
    "researchQuestionLink"
  )], is.null, logical(1))))
  testthat::expect_true(is.data.frame(out$evidenceTable))
  testthat::expect_false(any(vapply(out, is.null, logical(1))))
})

testthat::test_that("renderExplanationTeachingSummaryUi returns a UI object", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_response_noun_phrase") = "exam mark"
  audit = buildModelExplanationAudit(model)
  summary = buildExplanationTeachingSummary(audit = audit, model = model)
  ui = renderExplanationTeachingSummaryUi(summary)

  testthat::expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

testthat::test_that("renderTeachingSummaryText turns backticked names into code tags", {
  ui = renderTeachingSummaryText("The predictor `Test` is shown as a chip.")

  testthat::expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
  testthat::expect_true(any(vapply(ui$children, function(x) inherits(x, "shiny.tag") && identical(x$name, "code"), logical(1))))
})
