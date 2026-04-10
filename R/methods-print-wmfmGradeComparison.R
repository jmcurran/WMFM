#' Print a wmfmGradeComparison object
#'
#' @param x A `wmfmGradeComparison` object.
#' @param digits Number of digits for numeric output.
#' @param maxRows Maximum number of rows to print in each section.
#' @param ... Unused. Included for S3 compatibility.
#'
#' @return Invisibly returns `x`.
#' @export
print.wmfmGradeComparison = function(
    x,
    digits = 2,
    maxRows = 6,
    ...
) {

  if (!inherits(x, "wmfmGradeComparison")) {
    stop("`x` must inherit from `wmfmGradeComparison`.", call. = FALSE)
  }

  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }

  if (!is.numeric(maxRows) || length(maxRows) != 1 || is.na(maxRows) || maxRows < 1) {
    stop("`maxRows` must be a single positive number.", call. = FALSE)
  }

  fmt = function(v) {
    vapply(v, function(oneValue) {
      if (length(oneValue) != 1 || is.na(oneValue)) {
        return("NA")
      }

      formatC(oneValue, digits = digits, format = "f")
    }, character(1))
  }

  `%||%` = function(a, b) {
    if (is.null(a)) {
      b
    } else {
      a
    }
  }

  titleCaseMethod = function(method) {
    if (!is.character(method) || length(method) != 1 || is.na(method)) {
      return("unknown")
    }

    if (identical(method, "llm")) {
      return("LLM")
    }

    paste0(toupper(substr(method, 1, 1)), substring(method, 2))
  }

  getMethodBlock = function(sourceGrade, method) {
    if (inherits(sourceGrade, "wmfmGrade")) {
      return(sourceGrade$scores$byMethod[[method]] %||% NULL)
    }

    if (is.list(sourceGrade)) {
      for (obj in sourceGrade) {
        if (inherits(obj, "wmfmGrade") && method %in% names(obj$scores$byMethod %||% list())) {
          return(obj$scores$byMethod[[method]])
        }
      }
    }

    NULL
  }

  buildMethodLabel = function(method) {
    block = getMethodBlock(x$sourceGrade, method)
    label = titleCaseMethod(method)
    nRuns = block$nRuns %||% 1L

    if (identical(method, "llm") && isTRUE(nRuns > 1L)) {
      return(paste0(label, " (mean of ", nRuns, " runs)"))
    }

    label
  }

  buildDifferenceLabel = function() {
    methods = unique(c(x$summary$leftMethod, x$summary$rightMethod))

    if (all(c("deterministic", "llm") %in% methods)) {
      return("Difference (LLM - deterministic)")
    }

    paste0(
      "Difference (",
      titleCaseMethod(x$summary$rightMethod),
      " - ",
      titleCaseMethod(x$summary$leftMethod),
      ")"
    )
  }

  getDifferenceValue = function(leftValue, rightValue) {
    if (
      identical(x$summary$leftMethod, "deterministic") &&
        identical(x$summary$rightMethod, "llm")
    ) {
      return(unname(rightValue - leftValue))
    }

    if (
      identical(x$summary$leftMethod, "llm") &&
        identical(x$summary$rightMethod, "deterministic")
    ) {
      return(unname(leftValue - rightValue))
    }

    unname(rightValue - leftValue)
  }

  printSectionHeader = function(title) {
    cat("\n", title, "\n", sep = "")
    cat(strrep("-", nchar(title)), "\n", sep = "")
  }

  printKeyValueBlock = function(values) {
    labels = names(values)
    width = max(nchar(labels), na.rm = TRUE)

    for (i in seq_along(values)) {
      cat(format(labels[i], width = width, justify = "left"), ": ", values[[i]], "\n", sep = "")
    }
  }

  scaleValue = NA_real_

  if (inherits(x$sourceGrade, "wmfmGrade")) {
    scaleValue = x$sourceGrade$scoreScale %||% NA_real_
  } else if (is.list(x$sourceGrade) && length(x$sourceGrade) > 0) {
    scaleCandidates = vapply(
      x$sourceGrade,
      function(obj) {
        if (inherits(obj, "wmfmGrade")) {
          obj$scoreScale %||% NA_real_
        } else {
          NA_real_
        }
      },
      numeric(1)
    )

    uniqueScales = unique(scaleCandidates[!is.na(scaleCandidates)])
    if (length(uniqueScales) == 1) {
      scaleValue = uniqueScales[1]
    }
  }

  leftMethodLabel = buildMethodLabel(x$summary$leftMethod)
  rightMethodLabel = buildMethodLabel(x$summary$rightMethod)
  differenceLabel = buildDifferenceLabel()

  cat("WMFM grade comparison\n")
  cat("=====================\n")

  printSectionHeader("Methods compared")
  printKeyValueBlock(c(
    Left = leftMethodLabel,
    Right = rightMethodLabel
  ))

  printSectionHeader("Mark comparison")

  leftMarkValue = fmt(x$summary$leftMark)
  rightMarkValue = fmt(x$summary$rightMark)

  if (!is.na(scaleValue)) {
    leftMarkValue = paste0(leftMarkValue, " / ", fmt(scaleValue))
    rightMarkValue = paste0(rightMarkValue, " / ", fmt(scaleValue))
  }

  printKeyValueBlock(c(
    stats::setNames(leftMarkValue, leftMethodLabel),
    stats::setNames(rightMarkValue, rightMethodLabel),
    stats::setNames(fmt(getDifferenceValue(x$summary$leftMark, x$summary$rightMark)), differenceLabel)
  ))

  printSectionHeader("Overall score comparison")
  printKeyValueBlock(c(
    stats::setNames(fmt(x$summary$leftOverallScore), leftMethodLabel),
    stats::setNames(fmt(x$summary$rightOverallScore), rightMethodLabel),
    stats::setNames(
      fmt(getDifferenceValue(x$summary$leftOverallScore, x$summary$rightOverallScore)),
      differenceLabel
    )
  ))

  metricComparison = x$metricComparison
  if (is.data.frame(metricComparison) && nrow(metricComparison) > 0) {
    metricOrder = c(
      "factualScore",
      "inferenceScore",
      "completenessScore",
      "clarityScore",
      "calibrationScore"
    )

    metricRank = match(metricComparison$metric, metricOrder)
    metricRank[is.na(metricRank)] = length(metricOrder) + seq_len(sum(is.na(metricRank)))

    metricComparison = metricComparison[order(metricRank), , drop = FALSE]
    metricComparison = utils::head(metricComparison, maxRows)

    metricLabel = ifelse(
      is.na(metricComparison$label) | !nzchar(metricComparison$label),
      metricComparison$metric,
      metricComparison$label
    )

    maxText = ifelse(
      is.na(metricComparison$maxValue),
      "",
      paste0(" / ", fmt(metricComparison$maxValue))
    )

    metricDf = data.frame(
      Metric = paste0(metricLabel, maxText),
      left = fmt(metricComparison$leftValue),
      right = fmt(metricComparison$rightValue),
      difference = fmt(getDifferenceValue(metricComparison$leftValue, metricComparison$rightValue)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    names(metricDf)[2:4] = c(leftMethodLabel, rightMethodLabel, differenceLabel)

    printSectionHeader("Per-metric comparison")
    print(noquote(metricDf), row.names = FALSE, right = TRUE)
  }

  advisoryLeft = x$advisoryComparison$left
  advisoryRight = x$advisoryComparison$right

  hasLeftAdvisory = is.data.frame(advisoryLeft) && nrow(advisoryLeft) > 0
  hasRightAdvisory = is.data.frame(advisoryRight) && nrow(advisoryRight) > 0

  if (hasLeftAdvisory || hasRightAdvisory) {
    printSectionHeader("Additional rubric flags")

    printAdvisoryBlock = function(df, methodLabel) {
      cat(methodLabel, "\n", sep = "")

      df = utils::head(df, maxRows)
      lines = vapply(
        seq_len(nrow(df)),
        function(i) {
          paste0("* ", df$label[i], ": ", df$detail[i])
        },
        character(1)
      )

      cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }

    if (hasLeftAdvisory) {
      printAdvisoryBlock(advisoryLeft, leftMethodLabel)
    }

    if (hasRightAdvisory) {
      if (hasLeftAdvisory) {
        cat("\n")
      }
      printAdvisoryBlock(advisoryRight, rightMethodLabel)
    }
  }

  invisible(x)
}
