#' Rebuild WMFM repeated-run records without rerunning the LLM
#'
#' Recomputes extracted claim fields by re-running buildWmfmRunRecord() on
#' existing repeated-run outputs. This is useful when the extraction rules
#' change, for example when debugging overclaim detection.
#'
#' @param x A data.frame of run records or an object containing a runsDf element.
#' @param rescore Logical. Should scoreWmfmRepeatedRuns() be run after rebuilding?
#' @param preserveClass Logical. If x is a repeated-runs object, should its class
#'   be preserved on output?
#'
#' @return If x is a data.frame, returns a rebuilt data.frame.
#'   If x is a repeated-runs object, returns the same kind of object with runsDf
#'   replaced by rebuilt records and summary recomputed.
#' @export
rebuildWmfmRunRecords = function(
    x,
    rescore = TRUE,
    preserveClass = TRUE
) {

  extractRunsDf = function(obj) {
    if (is.data.frame(obj)) {
      return(obj)
    }

    if (is.list(obj) && "runsDf" %in% names(obj) && is.data.frame(obj$runsDf)) {
      return(obj$runsDf)
    }

    stop(
      "`x` must be a data.frame or an object containing a data.frame named `runsDf`.",
      call. = FALSE
    )
  }

  splitInteractionTerms = function(x) {
    if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
      return(character(0))
    }

    parts = unlist(strsplit(as.character(x), "\\|", fixed = FALSE))
    trimws(parts[nzchar(trimws(parts))])
  }

  rebuildOne = function(rowDf) {
    buildWmfmRunRecord(
      runId = rowDf$runId[[1]],
      exampleName = rowDf$exampleName[[1]],
      package = rowDf$package[[1]],
      modelType = rowDf$modelType[[1]],
      formula = rowDf$formula[[1]],
      equationsText = rowDf$equationsText[[1]],
      explanationText = rowDf$explanationText[[1]],
      errorMessage = rowDf$errorMessage[[1]],
      interactionTerms = splitInteractionTerms(rowDf$interactionTerms[[1]]),
      interactionMinPValue = rowDf$interactionMinPValue[[1]],
      interactionAlpha = rowDf$interactionAlpha[[1]]
    )
  }

  runsDf = extractRunsDf(x)

  rebuiltList = lapply(seq_len(nrow(runsDf)), function(i) {
    rebuildOne(runsDf[i, , drop = FALSE])
  })

  rebuiltDf = do.call(rbind, lapply(rebuiltList, as.data.frame))
  rownames(rebuiltDf) = NULL

  if (isTRUE(rescore)) {
    rebuiltDf = scoreWmfmRepeatedRuns(rebuiltDf)
  }

  if (is.data.frame(x)) {
    return(rebuiltDf)
  }

  out = x
  out$runsDf = rebuiltDf

  if ("summary" %in% names(out)) {
    out$summary = summariseWmfmRepeatedRuns(rebuiltDf)
  }

  if (isTRUE(preserveClass)) {
    class(out) = class(x)
  }

  out
}
