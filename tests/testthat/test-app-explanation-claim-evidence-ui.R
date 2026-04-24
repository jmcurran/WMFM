testthat::test_that("answer role notes are shown first in the UI list", {
  row = data.frame(
    claimId = "claim_1",
    sentenceIndex = 2L,
    claimText = "Overall, the model suggests higher Test scores are linked with higher exam marks.",
    claimTags = I(list(c("effect", "answer", "comparison"))),
    claimType = "answer",
    supportNotes = I(list(c(
      "explains how the response changes",
      "helps answer the research question by pulling the results together",
      "describes how groups are being compared"
    ))),
    supportNote = paste(
      "This sentence explains how the response changes.",
      "It also helps answer the research question by pulling the results together.",
      "It also describes how groups are being compared."
    ),
    evidenceCount = 2L,
    evidenceTypes = "mainEffect, comparison",
    evidenceLabels = "Main effect translation | Reference level",
    mappingMethod = "tag-first mapping",
    stringsAsFactors = FALSE
  )

  notes = buildExplanationClaimUiRoleNotes(row)

  testthat::expect_identical(
    notes[[1]],
    "helps answer the research question by pulling the results together"
  )
})

testthat::test_that("claim evidence cards clean answer formatting before display", {
  row = data.frame(
    claimId = "claim_1",
    sentenceIndex = 1L,
    claimText = "Answer: Overall, higher Test scores are linked with higher exam marks.",
    claimTags = I(list(c("effect", "answer"))),
    claimType = "answer",
    supportNotes = I(list("helps answer the research question by pulling the results together")),
    supportNote = "This sentence helps answer the research question.",
    evidenceCount = 1L,
    evidenceTypes = "mainEffect",
    evidenceLabels = "Main effect translation",
    mappingMethod = "tag-first mapping",
    stringsAsFactors = FALSE
  )

  html = as.character(renderExplanationClaimEvidenceCardUi(row))

  testthat::expect_match(html, "Overall, higher Test scores", fixed = TRUE)
  testthat::expect_no_match(html, "Answer:", fixed = TRUE)
})

testthat::test_that("developer feedback checkboxes are hidden by default", {
  row = data.frame(
    claimId = "claim_1",
    sentenceIndex = 1L,
    claimText = "Higher Test scores are linked with higher exam marks.",
    claimTags = I(list(c("effect"))),
    claimType = "effect",
    supportNotes = I(list("explains how the response changes")),
    supportNote = "This sentence explains how the response changes.",
    evidenceCount = 1L,
    evidenceTypes = "mainEffect",
    evidenceLabels = "Main effect translation",
    mappingMethod = "tag-first mapping",
    stringsAsFactors = FALSE
  )

  html = as.character(renderExplanationClaimEvidenceCardUi(row))

  testthat::expect_no_match(html, "Mark as incorrect", fixed = TRUE)
  testthat::expect_no_match(html, "developerFeedbackIncorrect_1", fixed = TRUE)
})

testthat::test_that("developer feedback checkboxes appear in developer mode", {
  row = data.frame(
    claimId = "claim_1",
    sentenceIndex = 1L,
    claimText = "Higher Test scores are linked with higher exam marks.",
    claimTags = I(list(c("effect"))),
    claimType = "effect",
    supportNotes = I(list("explains how the response changes")),
    supportNote = "This sentence explains how the response changes.",
    evidenceCount = 1L,
    evidenceTypes = "mainEffect",
    evidenceLabels = "Main effect translation",
    mappingMethod = "tag-first mapping",
    stringsAsFactors = FALSE
  )

  html = as.character(renderExplanationClaimEvidenceCardUi(
    row = row,
    developerMode = TRUE
  ))

  testthat::expect_match(html, "Mark as incorrect", fixed = TRUE)
  testthat::expect_match(html, "developerFeedbackIncorrect_1", fixed = TRUE)
})

testthat::test_that("claim evidence map passes developer mode to every card", {
  claimMap = list(
    claims = data.frame(
      claimId = c("claim_1", "claim_2"),
      sentenceIndex = c(1L, 2L),
      claimText = c(
        "Higher Test scores are linked with higher exam marks.",
        "This helps answer the research question."
      ),
      claimTags = I(list(c("effect"), c("answer"))),
      claimType = c("effect", "answer"),
      supportNotes = I(list(
        "explains how the response changes",
        "helps answer the research question by pulling the results together"
      )),
      supportNote = c(
        "This sentence explains how the response changes.",
        "This sentence helps answer the research question."
      ),
      evidenceCount = c(1L, 1L),
      evidenceTypes = c("mainEffect", "answer"),
      evidenceLabels = c("Main effect translation", "Research question framing"),
      mappingMethod = c("tag-first mapping", "tag-first mapping"),
      stringsAsFactors = FALSE
    )
  )

  html = as.character(renderExplanationClaimEvidenceUi(
    claimMap = claimMap,
    developerMode = TRUE
  ))

  testthat::expect_match(html, "developerFeedbackIncorrect_1", fixed = TRUE)
  testthat::expect_match(html, "developerFeedbackIncorrect_2", fixed = TRUE)
})
