test_that("contrast helper messages preserve existing notification wording", {
  expect_identical(buildSameContrastLevelsMessage(), "Please choose two different levels.")
  expect_identical(
    buildDuplicateContrastPairMessage(),
    "That contrast is already in the list (possibly reversed)."
  )
  expect_identical(
    buildNoContrastSelectionMessage(),
    "Select one or more contrasts to remove."
  )
  expect_identical(buildNoContrastPairsMessage(), "Add at least one contrast first.")
})

test_that("average contrast helper messages preserve existing wording", {
  expect_identical(
    buildAverageContrastEmptyGroupMessage(),
    "Drag at least one level into each average box."
  )
  expect_identical(
    buildAverageContrastInvalidLevelMessage(),
    "One or more selected levels are not valid for this factor."
  )
  expect_identical(
    buildAverageContrastOverlappingLevelMessage(),
    "A level cannot appear in both average boxes."
  )
})

test_that("custom contrast helper messages preserve existing wording", {
  expect_identical(
    buildInvalidCustomContrastWeightMessage(),
    "Custom weights must be numeric (decimals) or simple fractions like 1/2."
  )
  expect_identical(
    buildCustomContrastWeightsMustSumToZeroMessage(),
    "Custom weights must sum to 0."
  )
  expect_identical(
    buildCustomContrastTooFewWeightsMessage(),
    "Use at least two non-zero weights."
  )
})
