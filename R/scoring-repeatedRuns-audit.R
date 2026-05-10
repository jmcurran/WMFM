#' Summarise repeated developer scoring fixture stability
#'
#' Builds audit-only summary tables from developer scoring fixtures exported by the
#' developer scoring UI. This helper does not rescore explanations or change any
#' scoring decisions; it only summarises the scores and metric values already
#' present in the supplied fixtures.
#'
#' @param fixtures Named list of developer scoring fixture payloads.
#' @param metricLabels Character vector of metric labels to audit. When `NULL`,
#'   all metric labels found in the fixtures are included.
#' @param unstableSpreadThreshold Numeric mark spread at or above which an
#'   example is flagged as unstable.
#' @param lowMarkThreshold Numeric mark at or below which a run is flagged as a
#'   low-mark run.
#' @param metricSpreadThreshold Numeric metric-value spread at or above which a
#'   metric is flagged as unstable within an example.
#'
#' @return A list with `runSummary`, `exampleSummary`, `metricSummary`, and
#'   `unstableMetrics` data frames.
#' @export
summariseDeveloperScoringAudit = function(
    fixtures,
    metricLabels = NULL,
    unstableSpreadThreshold = 2,
    lowMarkThreshold = 4,
    metricSpreadThreshold = 1
) {
  if (!is.list(fixtures) || length(fixtures) == 0) {
    stop("`fixtures` must be a non-empty named list.", call. = FALSE)
  }

  fixtureNames = names(fixtures)

  if (is.null(fixtureNames)) {
    fixtureNames = rep("", length(fixtures))
  }

  fixtureNames = vapply(
    seq_along(fixtures),
    function(i) {
      fixtureName = fixtureNames[[i]]

      if (nzchar(fixtureName)) {
        return(fixtureName)
      }

      exampleName = fixtures[[i]]$appState$exampleName

      if (!is.null(exampleName) && nzchar(exampleName)) {
        return(exampleName)
      }

      paste0("fixture-", i)
    },
    character(1)
  )

  runSummary = buildDeveloperScoringRunAuditSummary(
    fixtures = fixtures,
    fixtureNames = fixtureNames,
    lowMarkThreshold = lowMarkThreshold
  )

  exampleSummary = buildDeveloperScoringExampleAuditSummary(
    runSummary = runSummary,
    unstableSpreadThreshold = unstableSpreadThreshold
  )

  metricSummary = buildDeveloperScoringMetricAuditSummary(
    fixtures = fixtures,
    fixtureNames = fixtureNames,
    metricLabels = metricLabels,
    metricSpreadThreshold = metricSpreadThreshold
  )

  unstableMetrics = metricSummary[metricSummary$unstableMetric, , drop = FALSE]

  list(
    runSummary = runSummary,
    exampleSummary = exampleSummary,
    metricSummary = metricSummary,
    unstableMetrics = unstableMetrics
  )
}

buildDeveloperScoringRunAuditSummary = function(
    fixtures,
    fixtureNames,
    lowMarkThreshold
) {
  runRows = unlist(
    lapply(
      seq_along(fixtures),
      function(i) {
        fixture = fixtures[[i]]
        runTable = fixture$repeated$runTable

        if (is.null(runTable) || length(runTable) == 0) {
          return(list())
        }

        lapply(
          seq_along(runTable),
          function(j) {
            row = runTable[[j]]
            mark = asDeveloperScoringNumeric(row$mark)
            overallScore = asDeveloperScoringNumeric(row$overallScore)

            data.frame(
              exampleName = fixtureNames[[i]],
              run = asDeveloperScoringInteger(row$run, fallback = j),
              status = asDeveloperScoringCharacter(row$status),
              scored = asDeveloperScoringLogical(row$scored),
              mark = mark,
              overallScore = overallScore,
              elapsedSeconds = asDeveloperScoringNumeric(row$elapsedSeconds),
              lowMarkRun = !is.na(mark) && mark <= lowMarkThreshold,
              stringsAsFactors = FALSE
            )
          }
        )
      }
    ),
    recursive = FALSE
  )

  if (length(runRows) == 0) {
    return(data.frame(
      exampleName = character(),
      run = integer(),
      status = character(),
      scored = logical(),
      mark = numeric(),
      overallScore = numeric(),
      elapsedSeconds = numeric(),
      lowMarkRun = logical(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, runRows)
}

buildDeveloperScoringExampleAuditSummary = function(
    runSummary,
    unstableSpreadThreshold
) {
  exampleNames = unique(runSummary$exampleName)

  rows = lapply(
    exampleNames,
    function(exampleName) {
      exampleRuns = runSummary[runSummary$exampleName == exampleName, , drop = FALSE]
      marks = exampleRuns$mark[!is.na(exampleRuns$mark)]
      overallScores = exampleRuns$overallScore[!is.na(exampleRuns$overallScore)]
      markSpread = calculateDeveloperScoringSpread(marks)
      overallScoreSpread = calculateDeveloperScoringSpread(overallScores)

      data.frame(
        exampleName = exampleName,
        runCount = nrow(exampleRuns),
        scoredRunCount = sum(isDeveloperScoringTRUEVector(exampleRuns$scored)),
        minMark = safeDeveloperScoringMin(marks),
        medianMark = safeDeveloperScoringMedian(marks),
        maxMark = safeDeveloperScoringMax(marks),
        markSpread = markSpread,
        minOverallScore = safeDeveloperScoringMin(overallScores),
        medianOverallScore = safeDeveloperScoringMedian(overallScores),
        maxOverallScore = safeDeveloperScoringMax(overallScores),
        overallScoreSpread = overallScoreSpread,
        lowMarkRunCount = sum(isDeveloperScoringTRUEVector(exampleRuns$lowMarkRun)),
        unstableExample = !is.na(markSpread) && markSpread >= unstableSpreadThreshold,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, rows)
}

buildDeveloperScoringMetricAuditSummary = function(
    fixtures,
    fixtureNames,
    metricLabels,
    metricSpreadThreshold
) {
  metricRows = buildDeveloperScoringMetricRows(
    fixtures = fixtures,
    fixtureNames = fixtureNames,
    metricLabels = metricLabels
  )

  if (nrow(metricRows) == 0) {
    return(data.frame(
      exampleName = character(),
      metricLabel = character(),
      runCount = integer(),
      minValue = numeric(),
      medianValue = numeric(),
      maxValue = numeric(),
      valueSpread = numeric(),
      unstableMetric = logical(),
      stringsAsFactors = FALSE
    ))
  }

  metricKeys = unique(paste(metricRows$exampleName, metricRows$metricLabel, sep = "\r"))
  rows = lapply(
    metricKeys,
    function(metricKey) {
      keyParts = strsplit(metricKey, "\r", fixed = TRUE)[[1]]
      exampleName = keyParts[[1]]
      metricLabel = keyParts[[2]]
      selectedRows = metricRows[
        metricRows$exampleName == exampleName & metricRows$metricLabel == metricLabel,
        ,
        drop = FALSE
      ]
      values = selectedRows$value[!is.na(selectedRows$value)]
      valueSpread = calculateDeveloperScoringSpread(values)

      data.frame(
        exampleName = exampleName,
        metricLabel = metricLabel,
        runCount = nrow(selectedRows),
        minValue = safeDeveloperScoringMin(values),
        medianValue = safeDeveloperScoringMedian(values),
        maxValue = safeDeveloperScoringMax(values),
        valueSpread = valueSpread,
        unstableMetric = !is.na(valueSpread) && valueSpread >= metricSpreadThreshold,
        stringsAsFactors = FALSE
      )
    }
  )

  summary = do.call(rbind, rows)
  summary[order(summary$exampleName, -summary$valueSpread, summary$metricLabel), , drop = FALSE]
}

buildDeveloperScoringMetricRows = function(
    fixtures,
    fixtureNames,
    metricLabels
) {
  allMetricRows = unlist(
    lapply(
      seq_along(fixtures),
      function(i) {
        runs = fixtures[[i]]$repeated$runs

        if (is.null(runs) || length(runs) == 0) {
          return(list())
        }

        lapply(
          seq_along(runs),
          function(j) {
            metrics = runs[[j]]$grade$metrics

            if (is.null(metrics) || length(metrics) == 0) {
              return(NULL)
            }

            metricRows = lapply(
              metrics,
              function(metric) {
                metricLabel = asDeveloperScoringCharacter(metric$label)

                if (!is.null(metricLabels) && !(metricLabel %in% metricLabels)) {
                  return(NULL)
                }

                data.frame(
                  exampleName = fixtureNames[[i]],
                  run = asDeveloperScoringInteger(runs[[j]]$run, fallback = j),
                  metricLabel = metricLabel,
                  value = asDeveloperScoringNumeric(metric$studentValue),
                  stringsAsFactors = FALSE
                )
              }
            )

            metricRows = Filter(Negate(is.null), metricRows)

            if (length(metricRows) == 0) {
              return(NULL)
            }

            do.call(rbind, metricRows)
          }
        )
      }
    ),
    recursive = FALSE
  )

  allMetricRows = Filter(Negate(is.null), allMetricRows)

  if (length(allMetricRows) == 0) {
    return(data.frame(
      exampleName = character(),
      run = integer(),
      metricLabel = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, allMetricRows)
}

asDeveloperScoringNumeric = function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(x[[1]]))
}

asDeveloperScoringInteger = function(x, fallback = NA_integer_) {
  value = asDeveloperScoringNumeric(x)

  if (is.na(value)) {
    return(as.integer(fallback))
  }

  as.integer(value)
}

asDeveloperScoringCharacter = function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

asDeveloperScoringLogical = function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    return(FALSE)
  }

  isTRUE(x[[1]])
}

isDeveloperScoringTRUEVector = function(x) {
  !is.na(x) & x
}

calculateDeveloperScoringSpread = function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x) - min(x)
}

safeDeveloperScoringMin = function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  min(x)
}

safeDeveloperScoringMedian = function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  stats::median(x)
}

safeDeveloperScoringMax = function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x)
}
