#' Build extracted-claim frequency data for a WMFM runs object
#'
#' @param x A `wmfmRuns` object.
#'
#' @return A data frame summarising claim frequencies.
#' @export
getWmfmRunsClaimsData = function(x) {
  if (!inherits(x, "wmfmRuns")) {
    stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  runsDf = do.call(
    rbind,
    lapply(
      x$runs,
      function(run) {
        as.data.frame(run, stringsAsFactors = FALSE)
      }
    )
  )

  rownames(runsDf) = NULL

  claimFields = c(
    "ciMention",
    "percentLanguageMention",
    "referenceGroupMention",
    "interactionMention",
    "uncertaintyMention",
    "usesInferentialLanguage",
    "usesDescriptiveOnlyLanguage",
    "overclaimDetected",
    "underclaimDetected",
    "conditionalLanguageMention",
    "comparisonLanguageMention",
    "outcomeMention",
    "predictorMention"
  )

  missingFields = setdiff(claimFields, names(runsDf))
  if (length(missingFields) > 0) {
    stop(
      "Missing expected claim fields: ",
      paste(missingFields, collapse = ", "),
      call. = FALSE
    )
  }

  out = lapply(
    claimFields,
    function(field) {
      values = as.logical(runsDf[[field]])

      nRuns = length(values)
      nPresent = sum(values %in% TRUE, na.rm = TRUE)

      data.frame(
        claim = field,
        nPresent = nPresent,
        nRuns = nRuns,
        proportionPresent = nPresent / nRuns,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, out)
}
