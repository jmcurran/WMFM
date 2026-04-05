#' Summarise metric-level comparison and deterministic ease
#'
#' Builds a metric-level summary combining cross-method disagreement
#' with deterministic stability/ease measures.
#'
#' Useful for investigating whether metrics that are easy to score
#' deterministically are also those with high disagreement.
#'
#' @param scores A `wmfmScores` object.
#' @param comparison A `wmfmScoreComparison` object.
#' @param deterministicMethod Name of deterministic method.
#'
#' @return A data.frame with one row per metric.
#' @export
summariseMetricComparison = function(
    scores,
    comparison,
    deterministicMethod = "deterministic"
) {

  if (!inherits(scores, "wmfmScores")) {
    stop("`scores` must inherit from `wmfmScores`.", call. = FALSE)
  }

  if (!inherits(comparison, "wmfmScoreComparison")) {
    stop("`comparison` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  registry = comparison$registry

  longDf = as.data.frame(scores, format = "long")
  longDf = longDf[longDf$method == deterministicMethod, , drop = FALSE]

  safeEntropy = function(x) {
    x = x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    p = as.numeric(table(x)) / length(x)
    -sum(p * log(p))
  }

  modalProp = function(x) {
    x = x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    max(table(x)) / length(x)
  }

  deterministicDf = do.call(rbind, lapply(seq_len(nrow(registry)), function(i) {

    m = registry$metricName[i]
    type = registry$metricType[i]

    if (!m %in% names(longDf)) return(NULL)

    vals = longDf[[m]]

    out = data.frame(
      metric = m,
      label = registry$label[i],
      group = registry$group[i],
      metricType = type,
      nUniqueDeterministic = length(unique(vals[!is.na(vals)])),
      modalProportionDeterministic = modalProp(as.character(vals)),
      entropyDeterministic = safeEntropy(as.character(vals)),
      stringsAsFactors = FALSE
    )

    if (type == "continuous") {
      num = suppressWarnings(as.numeric(vals))
      out$sdDeterministic = stats::sd(num, na.rm = TRUE)
    } else {
      out$sdDeterministic = NA_real_
    }

    out
  }))

  # ---- disagreement pieces ----
  ordinal = comparison$ordinalAgreement
  binary = comparison$binaryAgreement
  continuous = comparison$continuousAgreement

  cleanCols = function(df, cols) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df[, intersect(cols, names(df)), drop = FALSE]
  }

  ordinal = cleanCols(ordinal, c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual",
    "proportionAdjacent",
    "weightedKappa"
  ))

  binary = cleanCols(binary, c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual"
  ))

  continuous = cleanCols(continuous, c(
    "metric",
    "meanAbsoluteDifference",
    "correlation"
  ))

  disagreementDf = Reduce(function(x, y) merge(x, y, by = "metric", all = TRUE),
                          Filter(Negate(is.null), list(ordinal, binary, continuous))
  )

  if (is.null(disagreementDf)) {
    disagreementDf = data.frame(metric = deterministicDf$metric)
  }

  out = merge(deterministicDf, disagreementDf, by = "metric", all.x = TRUE)

  out
}
