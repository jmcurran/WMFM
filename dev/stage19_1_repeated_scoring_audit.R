#!/usr/bin/env Rscript

# Stage 19.1 repeated scoring audit.
#
# This script reads the Stage 18 developer scoring fixtures, summarises repeated
# scoring stability for SG-1 to SG-5, and writes audit-only CSV outputs. It does
# not change scoring logic or rescore explanations.

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

writeAuditTable = function(x, outputDir, fileName) {
  outputPath = file.path(outputDir, fileName)
  utils::write.csv(x, outputPath, row.names = FALSE)
  outputPath
}

packageRoot = findPackageRoot()
fixtureDir = file.path(packageRoot, "inst", "extdata", "developer-scoring", "stage18-9")
outputDir = file.path(packageRoot, "reports", "Stage 19 Repeated Scoring Audit")

if (!dir.exists(fixtureDir)) {
  stop(paste0("Fixture directory does not exist: ", fixtureDir), call. = FALSE)
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required to read developer scoring fixtures.", call. = FALSE)
}

dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

fixtures = readFixtures(fixtureDir)
audit = summariseDeveloperScoringAudit(fixtures)

writtenFiles = c(
  writeAuditTable(audit$runSummary, outputDir, "stage19_1_run_summary.csv"),
  writeAuditTable(audit$exampleSummary, outputDir, "stage19_1_example_summary.csv"),
  writeAuditTable(audit$metricSummary, outputDir, "stage19_1_metric_summary.csv"),
  writeAuditTable(audit$unstableMetrics, outputDir, "stage19_1_unstable_metrics.csv")
)

cat("Stage 19.1 repeated scoring audit complete.\n")
cat("Files written:\n")
cat(paste0("- ", writtenFiles, collapse = "\n"), "\n", sep = "")
