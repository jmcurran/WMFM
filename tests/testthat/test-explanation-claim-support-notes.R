testthat::test_that("buildExplanationClaimSupportNotes returns composable student-facing notes", {
  notes = buildExplanationClaimSupportNotes(
    claimTags = c("typicalCase", "effect", "uncertainty", "answer"),
    matchedEvidence = data.frame(stringsAsFactors = FALSE)
  )

  testthat::expect_identical(
    notes,
    c(
      "describes a typical case",
      "explains how the response changes",
      "shows uncertainty in the estimate",
      "helps answer the research question by pulling the results together"
    )
  )
})


testthat::test_that("buildExplanationClaimSupportNote composes multi-note sentences", {
  note = buildExplanationClaimSupportNote(
    claimTags = c("effect", "uncertainty"),
    matchedEvidence = data.frame(stringsAsFactors = FALSE)
  )

  testthat::expect_identical(
    note,
    paste(
      "This sentence explains how the response changes.",
      "It also shows uncertainty in the estimate."
    )
  )
})


testthat::test_that("getExplanationClaimSupportNoteForTag falls back for unknown tags", {
  testthat::expect_identical(
    getExplanationClaimSupportNoteForTag("unknownTag"),
    "provides supporting context"
  )
})
