#' Rebuild raw WMFM run records without rerunning the LLM
#'
#' Recomputes raw extracted fields for an existing `wmfmRuns` object by
#' re-running `buildWmfmRunRecord()` on the stored run metadata and generated
#' text. This is useful when extraction rules change and you want to refresh the
#' raw run records without generating new LLM outputs.
#'
#' This function is intentionally limited to rebuilding raw run records. It does
#' not rescore runs and does not compute summaries. If scoring is needed after
#' rebuilding, call `score()` on the returned object.
#'
#' @param x A `wmfmRuns` object.
#' @param preserveClass Logical. Should the class of `x` be preserved on the
#'   returned object? Defaults to `TRUE`.
#'
#' @return A rebuilt `wmfmRuns` object with refreshed `runs` records.
#' @export
rebuildWmfmRunRecords = function(
    x,
    preserveClass = TRUE
) {
  splitInteractionTerms = function(x) {
    if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
      return(character(0))
    }

    parts = unlist(strsplit(as.character(x), "\\|", fixed = FALSE))
    trimws(parts[nzchar(trimws(parts))])
  }

  getScalarField = function(runRecord, fieldName, default = NA) {
    if (!(fieldName %in% names(runRecord))) {
      return(default)
    }

    value = runRecord[[fieldName]]

    if (length(value) == 0) {
      return(default)
    }

    value[[1]]
  }

  rebuildOne = function(runRecord) {
    buildWmfmRunRecord(
      runId = getScalarField(runRecord, "runId"),
      exampleName = getScalarField(runRecord, "exampleName"),
      package = getScalarField(runRecord, "package"),
      modelType = getScalarField(runRecord, "modelType"),
      formula = getScalarField(runRecord, "formula"),
      equationsText = getScalarField(runRecord, "equationsText"),
      explanationText = getScalarField(runRecord, "explanationText"),
      errorMessage = getScalarField(runRecord, "errorMessage", NA_character_),
      interactionTerms = splitInteractionTerms(
        getScalarField(runRecord, "interactionTerms", NA_character_)
      ),
      interactionMinPValue = getScalarField(
        runRecord,
        "interactionMinPValue",
        NA_real_
      ),
      interactionAlpha = getScalarField(
        runRecord,
        "interactionAlpha",
        0.05
      )
    )
  }

  if (!inherits(x, "wmfmRuns")) {
    stop("`x` must inherit from `wmfmRuns`.", call. = FALSE)
  }

  if (!is.list(x$runs) || length(x$runs) == 0) {
    stop("`x$runs` must be a non-empty list of run records.", call. = FALSE)
  }

  rebuiltRuns = lapply(
    x$runs,
    function(runRecord) {
      if (!is.list(runRecord) || is.null(names(runRecord))) {
        stop(
          "Each element of `x$runs` must be a named run-record list.",
          call. = FALSE
        )
      }

      rebuildOne(runRecord)
    }
  )

  out = x
  out$runs = rebuiltRuns

  if ("summary" %in% names(out)) {
    out$summary = NULL
  }

  if (isTRUE(preserveClass)) {
    class(out) = class(x)
  }

  out
}
