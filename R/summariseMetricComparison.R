#' Summarise metric-level comparison and deterministic ease
#'
#' Builds a metric-level summary combining cross-method disagreement
#' with deterministic stability/ease measures.
#'
#' @param scores A `wmfmScores` object.
#' @param comparison A `wmfmScoreComparison` object.
#' @param deterministicMethod Name of deterministic method.
#' @param orderBy Optional ordering: NULL, `"disagreement"`, or `"ease"`.
#'
#' @return An object of class `metricComparisonSummary`.
#' @export
summariseMetricComparison = function(
  scores,
  comparison,
  deterministicMethod = "deterministic",
  orderBy = NULL
) {

  if (!inherits(scores, "wmfmScores")) {
    stop("`scores` must inherit from `wmfmScores`.", call. = FALSE)
  }

  if (!inherits(comparison, "wmfmScoreComparison")) {
    stop("`comparison` must inherit from `wmfmScoreComparison`.", call. = FALSE)
  }

  registry = comparison$registry

  if (is.null(registry) || !is.data.frame(registry) || nrow(registry) == 0) {
    stop("`comparison$registry` must be a non-empty data.frame.", call. = FALSE)
  }

  longDf = as.data.frame(scores, format = "long")
  longDf = longDf[longDf$method == deterministicMethod, , drop = FALSE]

  if (nrow(longDf) == 0) {
    stop("No rows found for deterministic method `", deterministicMethod, "`.", call. = FALSE)
  }

  safeEntropy = function(x) {
    x = x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_real_)
    }
    p = as.numeric(table(x)) / length(x)
    -sum(p * log(p))
  }

  modalProp = function(x) {
    x = x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_real_)
    }
    max(table(x)) / length(x)
  }

  deterministicDf = do.call(rbind, lapply(seq_len(nrow(registry)), function(i) {

    metricName = registry$metricName[i]
    metricType = registry$metricType[i]

    if (!metricName %in% names(longDf)) {
      return(NULL)
    }

    vals = longDf[[metricName]]

    data.frame(
      metric = metricName,
      label = registry$label[i],
      group = registry$group[i],
      metricType = metricType,
      modalProportionDeterministic = modalProp(as.character(vals)),
      entropyDeterministic = safeEntropy(as.character(vals)),
      nUniqueDeterministic = length(unique(vals[!is.na(vals)])),
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(deterministicDf) || nrow(deterministicDf) == 0) {
    stop("Could not derive deterministic metric summaries.", call. = FALSE)
  }

  cleanCols = function(df, cols) {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    df[, intersect(cols, names(df)), drop = FALSE]
  }

  ordinal = cleanCols(comparison$ordinalAgreement, c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual",
    "proportionAdjacent",
    "weightedKappa"
  ))

  binary = cleanCols(comparison$binaryAgreement, c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual"
  ))

  continuous = cleanCols(comparison$continuousAgreement, c(
    "metric",
    "meanAbsoluteDifference",
    "correlation"
  ))

  allCols = c(
    "metric",
    "meanAbsoluteDifference",
    "proportionEqual",
    "proportionAdjacent",
    "weightedKappa",
    "correlation"
  )

  standardiseCols = function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    for (nm in setdiff(allCols, names(df))) {
      df[[nm]] = NA_real_
    }

    df[, allCols, drop = FALSE]
  }

  disagreementList = Filter(
    Negate(is.null),
    list(
      standardiseCols(ordinal),
      standardiseCols(binary),
      standardiseCols(continuous)
    )
  )

  if (length(disagreementList) == 0) {
    disagreementDf = data.frame(
      metric = deterministicDf$metric,
      stringsAsFactors = FALSE
    )
  } else {
    disagreementDf = do.call(rbind, disagreementList)
    rownames(disagreementDf) = NULL
  }

  out = merge(
    deterministicDf,
    disagreementDf,
    by = "metric",
    all.x = TRUE,
    sort = FALSE
  )

  if (!is.null(orderBy)) {

    if (!orderBy %in% c("disagreement", "ease")) {
      stop("`orderBy` must be NULL, 'disagreement', or 'ease'.", call. = FALSE)
    }

    if (identical(orderBy, "disagreement") && "meanAbsoluteDifference" %in% names(out)) {
      out = out[order(-out$meanAbsoluteDifference, out$label), , drop = FALSE]
    }

    if (identical(orderBy, "ease")) {
      easeScore = -out$modalProportionDeterministic + out$entropyDeterministic
      out = out[order(easeScore, out$label), , drop = FALSE]
    }
  }

  rownames(out) = NULL

  structure(
    out,
    comparison = comparison,
    deterministicMethod = deterministicMethod,
    class = c("metricComparisonSummary", "data.frame")
  )
}
