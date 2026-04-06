#' Build claim-profile heatmap data for a WMFM runs object
#'
#' Extracts selected raw claim fields from a `wmfmRuns` object and reshapes them
#' into long format for heatmap plotting.
#'
#' @param x A `wmfmRuns` object.
#' @param fieldColumns Optional character vector of raw claim fields to include.
#'   If `NULL`, a default raw-only claim profile is used.
#' @param naLabel Character label used for missing values.
#' @param prettyFieldLabels Logical. Should field names be converted to more
#'   readable display labels?
#'
#' @return A data frame with columns `runId`, `field`, `value`, and
#'   `fieldLabel`.
#' @export
getWmfmRunsClaimProfileData = function(
    x,
    fieldColumns = NULL,
    naLabel = "(missing)",
    prettyFieldLabels = TRUE
) {
  coerceFieldColumn = function(x, naLabel) {
    if (is.logical(x)) {
      out = ifelse(is.na(x), naLabel, ifelse(x, "TRUE", "FALSE"))
      return(out)
    }

    if (is.factor(x)) {
      x = as.character(x)
    }

    if (is.numeric(x)) {
      if (all(is.na(x) | abs(x - round(x)) < .Machine$double.eps^0.5)) {
        x = as.character(as.integer(round(x)))
      } else {
        x = format(round(x, 2), trim = TRUE, nsmall = 0)
      }
    }

    x = as.character(x)
    x[is.na(x)] = naLabel
    x[trimws(x) == ""] = naLabel
    x
  }

  makeFieldLabel = function(x) {
    x = gsub("([a-z0-9])([A-Z])", "\\1 \\2", x)
    x = gsub("_", " ", x, fixed = TRUE)
    x
  }

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

  if (is.null(fieldColumns)) {
    fieldColumns = c(
      "effectDirectionClaim",
      "effectScaleClaim",
      "interactionSubstantiveClaim",
      "inferentialRegister",
      "uncertaintyTypeClaim",
      "referenceGroupMention",
      "interactionMention",
      "uncertaintyMention",
      "comparisonLanguageMention",
      "conditionalLanguageMention",
      "ciMention",
      "percentLanguageMention",
      "overclaimDetected",
      "underclaimDetected"
    )
  }

  missingFields = setdiff(fieldColumns, names(runsDf))
  if (length(missingFields) > 0) {
    stop(
      "Missing expected raw claim fields in `wmfmRuns`: ",
      paste(missingFields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!("runId" %in% names(runsDf))) {
    stop("`wmfmRuns` records must contain `runId`.", call. = FALSE)
  }

  out = do.call(
    rbind,
    lapply(
      fieldColumns,
      function(fieldName) {
        fieldLabel =
          if (isTRUE(prettyFieldLabels)) {
            makeFieldLabel(fieldName)
          } else {
            fieldName
          }

        data.frame(
          runId = as.integer(runsDf$runId),
          field = fieldName,
          fieldLabel = fieldLabel,
          value = coerceFieldColumn(runsDf[[fieldName]], naLabel = naLabel),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  out$fieldLabel = factor(
    out$fieldLabel,
    levels = rev(unique(out$fieldLabel))
  )

  out$runId = factor(
    out$runId,
    levels = sort(unique(as.integer(out$runId)))
  )

  out
}
