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
  testthat::expect_match(html, "There are 3 sentences in this explanation", fixed = TRUE)
  testthat::expect_match(html, "A sentence can play more than one role", fixed = TRUE)
  testthat::expect_match(html, "Sentence 1", fixed = TRUE)
  testthat::expect_match(html, "What this sentence is doing:", fixed = TRUE)
  testthat::expect_match(html, "Main model information behind it:", fixed = TRUE)
  testthat::expect_match(html, "Research question framing:", fixed = TRUE)
  testthat::expect_match(html, "the question the model is trying to answer", fixed = TRUE)
  testthat::expect_match(html, "Confidence interval rule:", fixed = TRUE)
  testthat::expect_match(html, "the uncertainty rule used to keep the wording cautious", fixed = TRUE)
  testthat::expect_match(html, "explains how the response changes", fixed = TRUE)
  testthat::expect_match(html, "shows uncertainty in the estimate", fixed = TRUE)
  testthat::expect_false(grepl("deterministic evidence", html, fixed = TRUE))
  testthat::expect_false(grepl("Supporting evidence:", html, fixed = TRUE))
  testthat::expect_false(grepl("mainEffect", html, fixed = TRUE))
  testthat::expect_false(grepl("Evidence used:", html, fixed = TRUE))
})


testthat::test_that("renderExplanationClaimEvidenceUi derives multiple role notes from claim tags", {
  claimMap = list(
    claims = data.frame(
      claimId = "claim_1",
      sentenceIndex = 1L,
      claimText = "The expected mark increases, but there is uncertainty around that increase.",
      claimTags = I(list(c("effect", "uncertainty"))),
      claimType = "mainEffect",
      supportNotes = I(list(character(0))),
      supportNote = "",
      evidenceCount = 2L,
      evidenceTypes = "mainEffect, confidenceInterval",
      evidenceLabels = "Main effect translation | Confidence interval rule",
      mappingMethod = "keyword-audit hybrid",
      stringsAsFactors = FALSE
    )
  )
  class(claimMap) = c("wmfmExplanationClaimEvidenceMap", class(claimMap))

  ui = renderExplanationClaimEvidenceUi(claimMap)
  html = as.character(ui)

  testthat::expect_match(html, "explains how the response changes", fixed = TRUE)
  testthat::expect_match(html, "shows uncertainty in the estimate", fixed = TRUE)
  testthat::expect_match(html, "Main model information behind it:", fixed = TRUE)
})


testthat::test_that("renderExplanationClaimEvidenceUi shows a single-sentence reading guide", {
  claimMap = list(
    claims = data.frame(
      claimId = "claim_1",
      sentenceIndex = 1L,
      claimText = "The expected mark increases with Test.",
      claimTags = I(list(c("effect"))),
      claimType = "mainEffect",
      supportNotes = I(list(c("explains how the response changes"))),
      supportNote = "This sentence explains how the response changes.",
      evidenceCount = 1L,
      evidenceTypes = "mainEffect",
      evidenceLabels = "Main effect translation",
      mappingMethod = "tag-first mapping",
      stringsAsFactors = FALSE
    )
  )
  class(claimMap) = c("wmfmExplanationClaimEvidenceMap", class(claimMap))

  ui = renderExplanationClaimEvidenceUi(claimMap)
  html = as.character(ui)

  testthat::expect_match(html, "There is 1 sentence in this explanation", fixed = TRUE)
  testthat::expect_match(html, "A sentence can play more than one role", fixed = TRUE)
})


testthat::test_that("app server renders the claim-evidence section before the teaching summary and AI tutor", {
  serverText = paste(deparse(body(appServer)), collapse = "\n")

  claimPos = regexpr('renderExplanationClaimEvidenceUi\\(claimMap\\)', serverText, perl = TRUE)[1]
  teachingPos = regexpr('renderExplanationTeachingSummaryUi\\(teachingSummary\\)', serverText, perl = TRUE)[1]
  tutorPos = regexpr('Optional AI tutor', serverText, fixed = TRUE)[1]

  testthat::expect_true(claimPos > 0)
  testthat::expect_true(teachingPos > 0)
  testthat::expect_true(tutorPos > 0)
  testthat::expect_lt(claimPos, teachingPos)
  testthat::expect_lt(teachingPos, tutorPos)
})

testthat::test_that("renderExplanationClaimEvidenceUi shows all mixed-role support notes", {
  claimMap = list(
    claims = data.frame(
      claimId = "claim_1",
      sentenceIndex = 1L,
      claimText = "Compared with SC, WA shows a steeper decline, but there is uncertainty around that difference.",
      claimTags = I(list(c("effect", "uncertainty", "comparison"))),
      claimType = "mainEffect",
      supportNotes = I(list(c(
        "explains how the response changes",
        "shows uncertainty in the estimate",
        "describes how groups are being compared"
      ))),
      supportNote = paste(
        "This sentence explains how the response changes.",
        "It also shows uncertainty in the estimate.",
        "It also describes how groups are being compared."
      ),
      evidenceCount = 3L,
      evidenceTypes = "mainEffect, confidenceInterval, comparison",
      evidenceLabels = "Main effect translation | Confidence interval rule | Reference level",
      mappingMethod = "tag-first mapping",
      stringsAsFactors = FALSE
    )
  )
  class(claimMap) = c("wmfmExplanationClaimEvidenceMap", class(claimMap))

  ui = renderExplanationClaimEvidenceUi(claimMap)
  html = as.character(ui)

  testthat::expect_match(html, "explains how the response changes", fixed = TRUE)
  testthat::expect_match(html, "shows uncertainty in the estimate", fixed = TRUE)
  testthat::expect_match(html, "describes how groups are being compared", fixed = TRUE)
  testthat::expect_match(html, "Reference level", fixed = TRUE)
  testthat::expect_match(html, "the comparison group used as the starting point", fixed = TRUE)
})
