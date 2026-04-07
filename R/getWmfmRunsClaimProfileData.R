#' Build claim-profile heatmap data for a WMFM runs object
#'
#' Extracts selected raw claim fields from a `wmfmRuns` object and reshapes them
#' into long format for heatmap plotting.
#'
#' Runs are intended to be shown on rows and claim fields on columns. Runs are
#' ordered by run purity. Fields can be ordered semantically or by field purity.
#'
#' @param x A `wmfmRuns` object.
#' @param fieldColumns Optional character vector of raw claim fields to include.
#'   If `NULL`, a default raw-only claim profile is used.
#' @param naLabel Character label used for missing values.
#' @param prettyFieldLabels Logical. Should field names be converted to more
#'   readable display labels?
#' @param fieldOrder Character. One of `"semantic"` or `"purity"`.
#'
#' @return A data frame with columns `runId`, `field`, `fieldLabel`, `value`,
#'   `modalValue`, `fieldPurity`, and `runPurity`.
#' @export
getWmfmRunsClaimProfileData = function(
    x,
    fieldColumns = NULL,
    naLabel = "(missing)",
    prettyFieldLabels = TRUE,
    fieldOrder = c("semantic", "purity")
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

  getModalValue = function(x) {
    xNoMissing = x[x != naLabel]

    if (length(xNoMissing) == 0) {
      return(naLabel)
    }

    tab = sort(table(xNoMissing), decreasing = TRUE)
    names(tab)[1]
  }

  fieldOrder = match.arg(fieldOrder)

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

  longDf = do.call(
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

  fieldKeep = vapply(
    split(longDf$value, longDf$field),
    function(values) {
      any(values != naLabel, na.rm = TRUE)
    },
    logical(1)
  )

  keepFields = names(fieldKeep)[fieldKeep]
  longDf = longDf[longDf$field %in% keepFields, , drop = FALSE]

  fieldStats = do.call(
    rbind,
    lapply(
      split(longDf, longDf$field),
      function(df) {
        modalValue = getModalValue(df$value)
        comparisonValues = ifelse(df$value == naLabel, NA, df$value == modalValue)
        fieldPurity = mean(comparisonValues, na.rm = TRUE)

        if (is.nan(fieldPurity)) {
          fieldPurity = NA_real_
        }

        data.frame(
          field = df$field[1],
          fieldLabel = df$fieldLabel[1],
          modalValue = modalValue,
          fieldPurity = fieldPurity,
          stringsAsFactors = FALSE
        )
      }
    )
  )

  longDf = merge(
    longDf,
    fieldStats,
    by = c("field", "fieldLabel"),
    all.x = TRUE,
    sort = FALSE
  )

  runStats = do.call(
    rbind,
    lapply(
      split(longDf, longDf$runId),
      function(df) {
        matchesModal = ifelse(df$value == naLabel, NA, df$value == df$modalValue)
        runPurity = mean(matchesModal, na.rm = TRUE)

        if (is.nan(runPurity)) {
          runPurity = NA_real_
        }

        data.frame(
          runId = as.integer(df$runId[1]),
          runPurity = runPurity,
          stringsAsFactors = FALSE
        )
      }
    )
  )

  longDf = merge(
    longDf,
    runStats,
    by = "runId",
    all.x = TRUE,
    sort = FALSE
  )

  if (identical(fieldOrder, "semantic")) {
    fieldOrderValues = fieldColumns[fieldColumns %in% unique(longDf$field)]
    fieldOrderLabels = unique(longDf[, c("field", "fieldLabel")])
    fieldOrderLabels = fieldOrderLabels[match(fieldOrderValues, fieldOrderLabels$field), "fieldLabel"]
  } else {
    fieldOrderLabels = fieldStats$fieldLabel[
      order(fieldStats$fieldPurity, fieldStats$fieldLabel)
    ]
  }

  runOrder = runStats$runId[order(-runStats$runPurity, runStats$runId)]

  longDf$fieldLabel = factor(longDf$fieldLabel, levels = fieldOrderLabels)
  longDf$runId = factor(longDf$runId, levels = rev(runOrder))

  longDf[order(longDf$runId, longDf$fieldLabel), ]
}
