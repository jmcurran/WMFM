#' Order WMFM legend values
#'
#' Returns legend values in a semantic order rather than alphabetical order.
#' This makes the legend easier to read by grouping related claim values
#' together.
#'
#' If `includeBreaks = TRUE`, blank entries are inserted between groups so the
#' legend displays with visual spacing.
#'
#' @param values Character vector of legend values to order.
#' @param includeBreaks Logical. Should blank spacer rows be inserted between
#'   semantic groups?
#'
#' @return A character vector of ordered legend values. If `includeBreaks =
#'   TRUE`, the returned vector may contain empty strings used as spacer rows.
#' @examples
#' orderWmfmLegendValues(c("unclear", "TRUE", "appropriate", "(missing)"))
#' orderWmfmLegendValues(
#'   c("unclear", "TRUE", "appropriate", "(missing)"),
#'   includeBreaks = TRUE
#' )
#' @export
orderWmfmLegendValues = function(values,
                                 includeBreaks = TRUE) {
  preferredGroups = list(
    c("TRUE", "FALSE"),
    c("difference_claimed", "difference_claimed_strongly"),
    c("appropriate", "too_weak", "overclaim", "overclaimed", "underclaim"),
    c("inferential", "descriptive", "descriptive_only"),
    c("mixed_or_both", "mixed_or_unclear", "unclear"),
    c("not_mentioned", "(missing)")
  )

  orderedGroups = lapply(
    preferredGroups,
    function(groupValues) {
      groupValues[groupValues %in% values]
    }
  )

  ordered = character(0)

  for (i in seq_along(orderedGroups)) {
    groupValues = orderedGroups[[i]]

    if (length(groupValues) == 0) {
      next
    }

    if (includeBreaks && length(ordered) > 0) {
      ordered = c(ordered, "")
    }

    ordered = c(ordered, groupValues)
  }

  extras = setdiff(values, unlist(preferredGroups, use.names = FALSE))

  if (length(extras) > 0) {
    if (includeBreaks && length(ordered) > 0) {
      ordered = c(ordered, "")
    }

    ordered = c(ordered, sort(extras))
  }

  ordered
}
