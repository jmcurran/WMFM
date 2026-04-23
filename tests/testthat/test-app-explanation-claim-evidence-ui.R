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
