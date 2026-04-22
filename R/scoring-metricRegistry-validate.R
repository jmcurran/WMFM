#' Validate a WMFM metric registry
#'
#' Checks that a WMFM metric registry has the required columns and
#' structurally valid values.
#'
#' @param registry A data frame describing WMFM metrics.
#'
#' @return Invisibly returns `TRUE` if the registry is valid.
#' @keywords internal
validateWmfmMetricRegistry = function(registry) {
  if (!is.data.frame(registry)) {
    stop("`registry` must be a data frame.", call. = FALSE)
  }

  requiredCols = c(
    "metricName",
    "metricType",
    "label",
    "group",
    "includeInComparison",
    "includeInStability",
    "includeInPlots",
    "orderedLevels"
  )

  missingCols = setdiff(requiredCols, names(registry))

  if (length(missingCols) > 0) {
    stop(
      "Metric registry is missing required columns: ",
      paste(missingCols, collapse = ", "),
      call. = FALSE
    )
  }

  validMetricTypes = c("binary", "ordinal", "continuous")
  badMetricTypes = setdiff(unique(registry$metricType), validMetricTypes)

  if (length(badMetricTypes) > 0) {
    stop(
      "Metric registry contains invalid metricType values: ",
      paste(badMetricTypes, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(registry$metricName) > 0) {
    stop("`metricName` values must be unique.", call. = FALSE)
  }

  ordinalRows = registry$metricType == "ordinal"

  if (any(ordinalRows)) {
    badOrdinal = vapply(
      registry$orderedLevels[ordinalRows],
      FUN = function(x) {
        is.null(x) || !is.character(x) || length(x) < 2
      },
      FUN.VALUE = logical(1)
    )

    if (any(badOrdinal)) {
      stop(
        "Each ordinal metric must define `orderedLevels` as a character vector with at least two values.",
        call. = FALSE
      )
    }
  }

  nonOrdinalRows = registry$metricType != "ordinal"

  if (any(nonOrdinalRows)) {
    badNonOrdinal = vapply(
      registry$orderedLevels[nonOrdinalRows],
      FUN = function(x) {
        !is.null(x)
      },
      FUN.VALUE = logical(1)
    )

    if (any(badNonOrdinal)) {
      stop(
        "Non-ordinal metrics must have `orderedLevels = NULL`.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
