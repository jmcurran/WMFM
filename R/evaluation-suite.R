#' List WMFM evaluation examples
#'
#' @param package Character. Package containing the examples.
#' @param includeTestExamples Logical. Include developer and test examples.
#'
#' @return A data frame containing stable example numbers, names, and evaluation metadata.
#' @export
listWMFMEvaluationExamples = function(package = "WMFM", includeTestExamples = FALSE) {
  details = listWMFMExampleDetails(
    package = package,
    includeTestExamples = includeTestExamples
  )

  records = lapply(seq_len(nrow(details)), function(i) {
    info = loadExampleSpec(details$exampleName[[i]], package = package)
    evaluation = info$spec$evaluation %||% list()
    data.frame(
      number = i,
      name = details$exampleName[[i]],
      datasetGroup = as.character(evaluation$datasetGroup %||% ""),
      taskType = as.character(evaluation$taskType %||% ""),
      intendedIntent = as.character(evaluation$intendedIntent %||% ""),
      suite = as.character(evaluation$suite %||% ""),
      stringsAsFactors = FALSE
    )
  })

  if (length(records) == 0) {
    return(data.frame(
      number = integer(0),
      name = character(0),
      datasetGroup = character(0),
      taskType = character(0),
      intendedIntent = character(0),
      suite = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, records)
}

#' Run a WMFM example evaluation suite
#'
#' @param numbers Optional integer positions from `listWMFMEvaluationExamples()`.
#' @param names Optional exact example names.
#' @param pattern Optional regular expression matched against example names.
#' @param taskType Optional evaluation task type.
#' @param suite Optional evaluation suite name.
#' @param outputDir Required directory for JSON and summary outputs.
#' @param package Character. Package containing the examples.
#' @param resume Logical. Skip examples with an existing successful JSON result.
#' @param overwrite Logical. Replace an existing output directory.
#' @param continueOnError Logical. Continue after an example fails.
#' @param showProgress Logical. Display example number, elapsed time, and estimated remaining time.
#' @param useExplanationCache Logical. Reuse cached LLM explanations.
#' @param ... Additional arguments passed to `runModel()`.
#'
#' @return Invisibly returns a list containing the manifest and result records.
#' @export
#' @importFrom jsonlite fromJSON toJSON write_json
#' @importFrom utils write.csv
runWMFMEvaluationSuite = function(
    numbers = NULL,
    names = NULL,
    pattern = NULL,
    taskType = NULL,
    suite = NULL,
    outputDir,
    package = "WMFM",
    resume = TRUE,
    overwrite = FALSE,
    continueOnError = TRUE,
    showProgress = TRUE,
    useExplanationCache = FALSE,
    ...) {
  selectors = c(
    !is.null(numbers),
    !is.null(names),
    !is.null(pattern),
    !is.null(taskType),
    !is.null(suite)
  )

  if (sum(selectors) != 1) {
    stop("Specify exactly one of `numbers`, `names`, `pattern`, `taskType`, or `suite`.", call. = FALSE)
  }

  outputDir = path.expand(trimws(as.character(outputDir %||% "")))
  if (!nzchar(outputDir)) {
    stop("`outputDir` must be supplied.", call. = FALSE)
  }

  examples = listWMFMEvaluationExamples(package = package)
  selected = examples

  if (!is.null(numbers)) {
    numbers = as.integer(numbers)
    if (anyNA(numbers) || any(numbers < 1L) || any(numbers > nrow(examples))) {
      stop("`numbers` contains positions outside the available example range.", call. = FALSE)
    }
    selected = examples[numbers, , drop = FALSE]
  } else if (!is.null(names)) {
    selected = examples[examples$name %in% as.character(names), , drop = FALSE]
  } else if (!is.null(pattern)) {
    selected = examples[grepl(pattern, examples$name, perl = TRUE), , drop = FALSE]
  } else if (!is.null(taskType)) {
    selected = examples[examples$taskType %in% as.character(taskType), , drop = FALSE]
  } else if (!is.null(suite)) {
    selected = examples[examples$suite %in% as.character(suite), , drop = FALSE]
  }

  if (nrow(selected) == 0) {
    stop("No examples matched the requested selection.", call. = FALSE)
  }

  if (dir.exists(outputDir) && isTRUE(overwrite)) {
    unlink(outputDir, recursive = TRUE, force = TRUE)
  }
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  startedAt = Sys.time()
  results = vector("list", nrow(selected))
  completedSeconds = numeric(0)

  for (i in seq_len(nrow(selected))) {
    exampleName = selected$name[[i]]
    safeName = gsub("[^A-Za-z0-9._-]+", "-", exampleName)
    resultPath = file.path(outputDir, paste0(safeName, ".json"))

    if (isTRUE(resume) && file.exists(resultPath)) {
      previous = tryCatch(fromJSON(resultPath, simplifyVector = FALSE), error = function(e) NULL)
      if (is.list(previous) && identical(previous$status, "success")) {
        results[[i]] = previous
        next
      }
    }

    if (isTRUE(showProgress)) {
      cat(sprintf("[%d/%d] %s\n", i, nrow(selected), exampleName))
    }

    exampleStart = Sys.time()
    record = tryCatch({
      info = loadExampleSpec(exampleName, package = package)
      result = runModel(
        data = info$data,
        formula = stats::as.formula(info$spec$formula),
        modelType = info$spec$modelType,
        dataContext = info$dataContext,
        researchQuestion = info$researchQuestion,
        followupQuestion = info$followupQuestion,
        printOutput = FALSE,
        useExplanationCache = useExplanationCache,
        ...
      )
      diagnostics = result$meta$followupDiagnostics %||% list()
      diagnostics$generatedExplanation = result$explanation %||% ""
      diagnostics$assembledPrompt = tryCatch(lmToExplanationPrompt(result$model), error = function(e) "")
      payload = fromJSON(buildExplanationPromptDiagnosticsJson(diagnostics), simplifyVector = FALSE)
      list(
        status = "success",
        exampleNumber = selected$number[[i]],
        exampleName = exampleName,
        datasetGroup = selected$datasetGroup[[i]],
        taskType = selected$taskType[[i]],
        intendedIntent = selected$intendedIntent[[i]],
        suite = selected$suite[[i]],
        packageVersion = as.character(utils::packageVersion(package)),
        diagnostics = payload,
        errorMessage = NULL
      )
    }, error = function(e) {
      list(
        status = "error",
        exampleNumber = selected$number[[i]],
        exampleName = exampleName,
        datasetGroup = selected$datasetGroup[[i]],
        taskType = selected$taskType[[i]],
        intendedIntent = selected$intendedIntent[[i]],
        suite = selected$suite[[i]],
        packageVersion = tryCatch(as.character(utils::packageVersion(package)), error = function(e2) ""),
        diagnostics = list(),
        errorMessage = conditionMessage(e)
      )
    })

    elapsed = as.numeric(difftime(Sys.time(), exampleStart, units = "secs"))
    record$elapsedSeconds = elapsed
    record$startedAt = format(exampleStart, "%Y-%m-%dT%H:%M:%S%z")
    record$finishedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    write_json(record, resultPath, pretty = TRUE, auto_unbox = TRUE, null = "null")
    results[[i]] = record
    completedSeconds = c(completedSeconds, elapsed)

    if (isTRUE(showProgress)) {
      remaining = nrow(selected) - i
      eta = if (remaining > 0) mean(completedSeconds) * remaining else 0
      cat(sprintf("Completed in %.1f seconds; estimated remaining %.1f seconds.\n", elapsed, eta))
    }

    if (identical(record$status, "error") && !isTRUE(continueOnError)) {
      stop(record$errorMessage, call. = FALSE)
    }
  }

  results = Filter(Negate(is.null), results)
  manifest = list(
    startedAt = format(startedAt, "%Y-%m-%dT%H:%M:%S%z"),
    finishedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    package = package,
    packageVersion = as.character(utils::packageVersion(package)),
    selectedExamples = selected$name,
    resultCount = length(results)
  )
  write_json(manifest, file.path(outputDir, "run-manifest.json"), pretty = TRUE, auto_unbox = TRUE)

  jsonl = vapply(results, function(x) toJSON(x, auto_unbox = TRUE, null = "null"), character(1))
  writeLines(jsonl, file.path(outputDir, "results.jsonl"), useBytes = TRUE)

  summaryData = data.frame(
    number = vapply(results, function(x) as.integer(x$exampleNumber), integer(1)),
    example = vapply(results, function(x) as.character(x$exampleName), character(1)),
    datasetGroup = vapply(results, function(x) as.character(x$datasetGroup), character(1)),
    taskType = vapply(results, function(x) as.character(x$taskType), character(1)),
    intendedIntent = vapply(results, function(x) as.character(x$intendedIntent), character(1)),
    status = vapply(results, function(x) as.character(x$status), character(1)),
    detectedIntent = vapply(results, function(x) as.character(x$diagnostics$predictionPayload$predictionIntent %||% ""), character(1)),
    elapsedSeconds = vapply(results, function(x) as.numeric(x$elapsedSeconds), numeric(1)),
    errorMessage = vapply(results, function(x) as.character(x$errorMessage %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
  write.csv(summaryData, file.path(outputDir, "results-summary.csv"), row.names = FALSE)

  invisible(list(manifest = manifest, results = results, summary = summaryData))
}
