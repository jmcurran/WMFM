#' Assess stability of WMFM score results
#'
#' Summarises within-method stability of scores stored in a `wmfmScores` object.
#' Stability is summarised separately for binary, ordinal, and continuous score
#' fields.
#'
#' @param x A `wmfmScores` object.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return An object of class `wmfmScoreStability`.
#' @export
stability.wmfmScores = function(
    x,
    ...
) {

  if (!inherits(x, "wmfmScores")) {
    stop("`x` must inherit from `wmfmScores`.", call. = FALSE)
  }

  df = as.data.frame(x, format = "long")

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("No scoring rows are available for stability assessment.", call. = FALSE)
  }

  methods = x$methods %||% character(0)

  binaryFields = c(
    "fatalFlawDetected",
    "overallPass"
  )

  ordinalFields = c(
    "effectDirectionCorrect",
    "effectScaleAppropriate",
    "referenceGroupHandledCorrectly",
    "interactionCoverageAdequate",
    "interactionSubstantiveCorrect",
    "uncertaintyHandlingAppropriate",
    "inferentialRegisterAppropriate",
    "mainEffectCoverageAdequate",
    "referenceGroupCoverageAdequate",
    "clarityAdequate",
    "numericExpressionAdequate",
    "comparisonStructureClear"
  )

  continuousFields = c(
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore",
    "overallScore"
  )

  summarizeBinaryField = function(values, methodName, field) {
    values = as.logical(values)
    values = values[!is.na(values)]

    if (length(values) == 0) {
      return(NULL)
    }

    counts = table(values)
    modalCount = max(counts)

    data.frame(
      method = methodName,
      metric = field,
      n = length(values),
      trueRate = mean(values),
      falseRate = mean(!values),
      modalValue = names(counts)[which.max(counts)],
      modalProportion = modalCount / length(values),
      variesAcrossRuns = length(unique(values)) > 1,
      stringsAsFactors = FALSE
    )
  }

  summarizeOrdinalField = function(values, methodName, field) {
    values = suppressWarnings(as.numeric(values))
    values = values[!is.na(values)]

    if (length(values) == 0) {
      return(NULL)
    }

    counts = table(values)
    modalCount = max(counts)

    data.frame(
      method = methodName,
      metric = field,
      n = length(values),
      mean = mean(values),
      sd = stats::sd(values),
      min = min(values),
      max = max(values),
      range = max(values) - min(values),
      modalValue = as.character(names(counts)[which.max(counts)]),
      modalProportion = modalCount / length(values),
      variesAcrossRuns = length(unique(values)) > 1,
      stringsAsFactors = FALSE
    )
  }

  summarizeContinuousField = function(values, methodName, field) {
    values = suppressWarnings(as.numeric(values))
    values = values[!is.na(values)]

    if (length(values) == 0) {
      return(NULL)
    }

    meanValue = mean(values)
    sdValue = stats::sd(values)

    data.frame(
      method = methodName,
      metric = field,
      n = length(values),
      mean = meanValue,
      sd = sdValue,
      min = min(values),
      max = max(values),
      range = max(values) - min(values),
      stringsAsFactors = FALSE
    )
  }

  collectSummaries = function(fields, summarizer) {
    pieces = list()
    pieceIndex = 0L

    for (methodName in methods) {
      methodDf = df[df$method == methodName, , drop = FALSE]

      if (nrow(methodDf) == 0) {
        next
      }

      for (field in fields) {
        if (!field %in% names(methodDf)) {
          next
        }

        result = summarizer(
          values = methodDf[[field]],
          methodName = methodName,
          field = field
        )

        if (!is.null(result)) {
          pieceIndex = pieceIndex + 1L
          pieces[[pieceIndex]] = result
        }
      }
    }

    if (length(pieces) == 0) {
      return(data.frame())
    }

    out = do.call(rbind, pieces)
    rownames(out) = NULL
    out
  }

  binaryStability = collectSummaries(binaryFields, summarizeBinaryField)
  ordinalStability = collectSummaries(ordinalFields, summarizeOrdinalField)
  continuousStability = collectSummaries(continuousFields, summarizeContinuousField)

  overallSummary = list()

  if (nrow(continuousStability) > 0 && "overallScore" %in% continuousStability$metric) {
    overallSummary$overallScore = continuousStability[
      continuousStability$metric == "overallScore",
      ,
      drop = FALSE
    ]
  }

  out = list(
    methods = methods,
    nRuns = length(x$runIds %||% integer(0)),
    binaryStability = binaryStability,
    ordinalStability = ordinalStability,
    continuousStability = continuousStability,
    overallSummary = overallSummary
  )

  class(out) = c("wmfmScoreStability", class(out))
  out
}
