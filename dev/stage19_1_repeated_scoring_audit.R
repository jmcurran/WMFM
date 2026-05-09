#!/usr/bin/env Rscript

# Stage 19 repeated scoring audit.
#
# This script reads developer scoring fixture JSON files, summarises repeated
# scoring stability, and writes audit-only CSV outputs. It does not change
# scoring logic or call an LLM.

findPackageRoot = function(path = getwd()) {
  currentPath = normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(currentPath, "DESCRIPTION"))) {
      return(currentPath)
    }

    parentPath = dirname(currentPath)

    if (identical(parentPath, currentPath)) {
      stop("Could not find package root containing DESCRIPTION.", call. = FALSE)
    }

    currentPath = parentPath
  }
}

normalisePathForMessage = function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

printUsage = function() {
  cat(
    paste(
      "Usage:",
      "  Rscript dev/stage19_1_repeated_scoring_audit.R [options]",
      "",
      "Options:",
      "  --fixture-dir PATH     Directory containing developer scoring JSON files.",
      "  --output-dir PATH      Directory where audit CSV files should be written.",
      "  --output-prefix TEXT   Prefix for generated CSV filenames.",
      "  --help, -h             Show this help message.",
      "",
      "Defaults:",
      "  --fixture-dir inst/extdata/developer-scoring/stage18-9",
      "  --output-dir reports/Stage 19 Repeated Scoring Audit",
      "  --output-prefix stage19_1",
      sep = "\n"
    ),
    "\n"
  )
}

parseAuditArgs = function(args = commandArgs(trailingOnly = TRUE), packageRoot = findPackageRoot()) {
  options = list(
    fixtureDir = file.path(packageRoot, "inst", "extdata", "developer-scoring", "stage18-9"),
    outputDir = file.path(packageRoot, "reports", "Stage 19 Repeated Scoring Audit"),
    outputPrefix = "stage19_1"
  )

  i = 1
  while (i <= length(args)) {
    arg = args[[i]]

    if (arg %in% c("--help", "-h")) {
      printUsage()
      quit(save = "no", status = 0)
    }

    if (arg %in% c("--fixture-dir", "--output-dir", "--output-prefix")) {
      if (i == length(args)) {
        stop(paste0("Missing value for ", arg, "."), call. = FALSE)
      }

      value = args[[i + 1]]

      if (!nzchar(value)) {
        stop(paste0("Empty value supplied for ", arg, "."), call. = FALSE)
      }

      if (identical(arg, "--fixture-dir")) {
        options$fixtureDir = value
      } else if (identical(arg, "--output-dir")) {
        options$outputDir = value
      } else if (identical(arg, "--output-prefix")) {
        options$outputPrefix = value
      }

      i = i + 2
      next
    }

    stop(paste0("Unknown argument: ", arg), call. = FALSE)
  }

  if (!grepl("^[A-Za-z0-9_.-]+$", options$outputPrefix)) {
    stop(
      "Output prefix must contain only letters, numbers, underscores, dots, or hyphens.",
      call. = FALSE
    )
  }

  options$fixtureDir = normalisePathForMessage(options$fixtureDir)
  options$outputDir = normalisePathForMessage(options$outputDir)
  options
}

readFixture = function(file) {
  jsonlite::fromJSON(file, simplifyVector = FALSE)
}

readFixtures = function(fixtureDir) {
  files = list.files(fixtureDir, pattern = "[.]json$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No developer scoring fixture JSON files were found.", call. = FALSE)
  }

  fixtures = lapply(sort(files), readFixture)
  names(fixtures) = vapply(
    fixtures,
    function(fixture) {
      exampleName = fixture$appState$exampleName

      if (!is.null(exampleName) && nzchar(exampleName)) {
        return(exampleName)
      }

      NA_character_
    },
    character(1)
  )

  fixtures
}

writeAuditTable = function(x, outputDir, outputPrefix, suffix) {
  fileName = paste0(outputPrefix, "_", suffix, ".csv")
  outputPath = file.path(outputDir, fileName)
  utils::write.csv(x, outputPath, row.names = FALSE)
  outputPath
}

ensureAuditFunctionAvailable = function(packageRoot) {
  if (exists("summariseDeveloperScoringAudit", mode = "function")) {
    return(invisible(TRUE))
  }

  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop(
      paste(
        "The summariseDeveloperScoringAudit() function is not loaded,",
        "and devtools is not available to load the package."
      ),
      call. = FALSE
    )
  }

  devtools::load_all(packageRoot, quiet = TRUE)

  if (!exists("summariseDeveloperScoringAudit", mode = "function")) {
    stop("Could not load summariseDeveloperScoringAudit().", call. = FALSE)
  }

  invisible(TRUE)
}

runRepeatedScoringAudit = function(args = commandArgs(trailingOnly = TRUE)) {
  packageRoot = findPackageRoot()
  auditOptions = parseAuditArgs(args = args, packageRoot = packageRoot)

  if (!dir.exists(auditOptions$fixtureDir)) {
    stop(
      paste0("Fixture directory does not exist: ", auditOptions$fixtureDir),
      call. = FALSE
    )
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required to read developer scoring fixtures.", call. = FALSE)
  }

  ensureAuditFunctionAvailable(packageRoot)

  dir.create(auditOptions$outputDir, recursive = TRUE, showWarnings = FALSE)

  fixtures = readFixtures(auditOptions$fixtureDir)
  audit = summariseDeveloperScoringAudit(fixtures)

  writtenFiles = c(
    writeAuditTable(audit$runSummary, auditOptions$outputDir, auditOptions$outputPrefix, "run_summary"),
    writeAuditTable(audit$exampleSummary, auditOptions$outputDir, auditOptions$outputPrefix, "example_summary"),
    writeAuditTable(audit$metricSummary, auditOptions$outputDir, auditOptions$outputPrefix, "metric_summary"),
    writeAuditTable(audit$unstableMetrics, auditOptions$outputDir, auditOptions$outputPrefix, "unstable_metrics")
  )

  cat("Repeated scoring audit complete.\n")
  cat("Fixture directory: ", auditOptions$fixtureDir, "\n", sep = "")
  cat("Output directory: ", auditOptions$outputDir, "\n", sep = "")
  cat("Output prefix: ", auditOptions$outputPrefix, "\n", sep = "")
  cat("Files written:\n")
  cat(paste0("- ", writtenFiles, collapse = "\n"), "\n", sep = "")

  invisible(writtenFiles)
}

runRepeatedScoringAudit()
