#' Summarise repeated runs
#'
#' @param runsDf Data frame of runs.
#'
#' @return List summary.
#' @export
summariseWmfmRepeatedRuns = function(runsDf) {

  valid = runsDf$normalizedExplanation[!is.na(runsDf$normalizedExplanation)]

  if (length(valid) == 0) {
    return(list(nRuns = nrow(runsDf)))
  }

  tbl = sort(table(valid), decreasing = TRUE)

  list(
    nRuns = nrow(runsDf),
    nUnique = length(tbl),
    mostCommonCount = tbl[1],
    stability = tbl[1] / length(valid),
    meanWords = mean(runsDf$wordCount, na.rm = TRUE)
  )
}
