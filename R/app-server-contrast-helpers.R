#' Build message for identical pairwise contrast levels.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildSameContrastLevelsMessage = function() {
  "Please choose two different levels."
}

#' Build message for duplicate pairwise contrast requests.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildDuplicateContrastPairMessage = function() {
  "That contrast is already in the list (possibly reversed)."
}

#' Build message for missing contrast removal selection.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildNoContrastSelectionMessage = function() {
  "Select one or more contrasts to remove."
}

#' Build message for missing pairwise contrast entries.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildNoContrastPairsMessage = function() {
  "Add at least one contrast first."
}

#' Build message for empty average contrast groups.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildAverageContrastEmptyGroupMessage = function() {
  "Drag at least one level into each average box."
}

#' Build message for invalid average contrast levels.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildAverageContrastInvalidLevelMessage = function() {
  "One or more selected levels are not valid for this factor."
}

#' Build message for overlapping average contrast levels.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildAverageContrastOverlappingLevelMessage = function() {
  "A level cannot appear in both average boxes."
}

#' Build message for invalid custom contrast weights.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildInvalidCustomContrastWeightMessage = function() {
  "Custom weights must be numeric (decimals) or simple fractions like 1/2."
}

#' Build message for non-zero custom contrast weight sums.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildCustomContrastWeightsMustSumToZeroMessage = function() {
  "Custom weights must sum to 0."
}

#' Build message for too few non-zero custom contrast weights.
#'
#' @return A character scalar notification message.
#'
#' @keywords internal
buildCustomContrastTooFewWeightsMessage = function() {
  "Use at least two non-zero weights."
}
