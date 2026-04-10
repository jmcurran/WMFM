#' Extract contextual run-level information for metric diagnosis
#'
#' Internal helper for \code{diagnose()} that attempts to recover
#' explanation text and diagnostic context. Preference is given to the
#' original runs object, because \code{wmfmScores} retains score fields
#' but not the full raw explanation record.
#'
#' @param scores A \code{wmfmScores} object.
#' @param metric A single metric name.
#' @param runs Optional \code{wmfmRuns} object or compatible list/data.frame.
#'
#' @return A data frame keyed by \code{runId}, or \code{NULL}.
#'
#' @keywords internal
extractMetricDiagnosisContext = function(scores, metric, runs = NULL) {
  asRunsDf = function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    if (is.data.frame(x)) {
      out = x
      if (!"runId" %in% names(out)) {
        out$runId = seq_len(nrow(out))
      }
      return(out)
    }

    if (inherits(x, "wmfmRuns") && !is.null(x$runs)) {
      out = do.call(
        rbind,
        lapply(x$runs, function(run) {
          as.data.frame(run, stringsAsFactors = FALSE)
        })
      )
      rownames(out) = NULL
      out$runId = seq_len(nrow(out))
      return(out)
    }

    if (is.list(x) && !is.null(x$runsDf) && is.data.frame(x$runsDf)) {
      out = x$runsDf
      if (!"runId" %in% names(out)) {
        out$runId = seq_len(nrow(out))
      }
      return(out)
    }

    NULL
  }

  runsDf = asRunsDf(runs)

  if (is.null(runsDf)) {
    return(NULL)
  }

  keepCols = intersect(
    c(
      "runId",
      "explanationText",
      "explanation",
      "formula",
      "equationsText",
      "interactionTerms",
      "interactionMinPValue",
      "effectDirectionClaim",
      "effectScaleClaim",
      "interactionSubstantiveClaim",
      "inferentialRegister",
      "percentLanguageMention",
      "comparisonLanguageMention",
      "conditionalLanguageMention",
      "referenceGroupMention",
      "uncertaintyMention",
      "ciMention",
      "overclaimDetected",
      "underclaimDetected",
      "hasInteractionTerms",
      "expectedEffectScale",
      "expectedEffectDirection"
    ),
    names(runsDf)
  )

  if (length(keepCols) < 1L) {
    return(NULL)
  }

  runsDf[, keepCols, drop = FALSE]
}
