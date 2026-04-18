testthat::test_that("buildExplanationTeachingSummary returns a stable student-facing structure", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"

  audit = buildModelExplanationAudit(model)
  out = buildExplanationTeachingSummary(audit = audit, model = model)

  testthat::expect_s3_class(out, "wmfmExplanationTeachingSummary")
  testthat::expect_named(
    out,
    c(
      "interpretationScale",
      "baselineChoice",
      "xChangeDescription",
      "mainEffectDescription",
      "uncertaintySummary",
      "evidenceTable",
      "researchQuestionLink"
    )
  )
  testthat::expect_true(all(vapply(out[1:5], is.character, logical(1))))
  testthat::expect_true(all(vapply(out[1:5], function(x) length(x) == 1 && !is.na(x) && nzchar(x), logical(1))))
  testthat::expect_true(is.data.frame(out$evidenceTable))
  testthat::expect_true(nrow(out$evidenceTable) >= 4)
  testthat::expect_true(all(c("section", "summary") %in% names(out$evidenceTable)))
  testthat::expect_match(out$baselineChoice, "pretending every variable begins at 0|starting value")
  testthat::expect_match(out$xChangeDescription, "goes up by 1 unit")
  testthat::expect_match(out$xChangeDescription, "`Test`")
  testthat::expect_match(out$researchQuestionLink, "Does Test help explain Exam\\?")
})

testthat::test_that("buildExplanationTeachingSummary fills all fields when no research question is supplied", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  audit = buildModelExplanationAudit(model)
  out = buildExplanationTeachingSummary(audit = audit, model = model, researchQuestion = NULL)

  testthat::expect_true(all(!vapply(out[c(
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

testthat::test_that("renderExplanationTeachingSummaryUi returns a UI object with code-style variable chips", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  audit = buildModelExplanationAudit(model)
  summary = buildExplanationTeachingSummary(audit = audit, model = model)
  ui = renderExplanationTeachingSummaryUi(summary)
  rendered = htmltools::renderTags(ui)$html

  testthat::expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
  testthat::expect_match(rendered, "<code")
  testthat::expect_match(rendered, "Test")
  testthat::expect_match(rendered, "How the outcome was described")
})
