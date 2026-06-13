#!/usr/bin/env Rscript

recognizedPrefixes = c(
  "api",
  "app",
  "class",
  "examples",
  "explain",
  "methods",
  "model",
  "plot",
  "prompt",
  "scoring",
  "text",
  "utils"
)

args = commandArgs(trailingOnly = TRUE)
outputPath = ""
if (length(args) >= 2L && args[[1L]] == "--output") {
  outputPath = args[[2L]]
}

countLines = function(path) {
  length(readLines(path, warn = FALSE))
}

fileBaseName = function(path) {
  basename(path)
}

sourcePrefix = function(path) {
  stem = sub("[.]R$", "", fileBaseName(path))
  parts = strsplit(stem, "-", fixed = TRUE)[[1L]]
  parts[[1L]]
}

isMixedStyleName = function(path) {
  stem = sub("[.]R$", "", fileBaseName(path))
  grepl("[a-z][A-Z]", stem)
}

testFileKind = function(path) {
  name = basename(path)
  if (startsWith(name, "test-")) {
    return("test")
  }
  if (startsWith(name, "helper-")) {
    return("helper")
  }
  "other"
}

devFileKind = function(path) {
  name = basename(path)
  if (startsWith(name, "stage")) {
    return("stage-context")
  }
  if (startsWith(name, "wmfm_")) {
    return("wmfm-context")
  }
  if (endsWith(name, "_context.md")) {
    return("context")
  }
  if (endsWith(name, "-context.md")) {
    return("context")
  }
  if (endsWith(name, "_audit.md")) {
    return("audit")
  }
  if (endsWith(name, "-audit.md")) {
    return("audit")
  }
  if (endsWith(name, ".R")) {
    return("dev-script")
  }
  "other"
}

markdownTable = function(data, columns) {
  if (nrow(data) == 0L) {
    return("No files found.")
  }

  lines = c(
    paste0("| ", paste(columns, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(columns)), collapse = " | "), " |")
  )

  for (rowIndex in seq_len(nrow(data))) {
    values = vapply(columns, function(column) {
      as.character(data[[column]][[rowIndex]])
    }, character(1L))
    lines = c(lines, paste0("| ", paste(values, collapse = " | "), " |"))
  }

  lines
}

relativePath = function(path) {
  absolutePath = normalizePath(path, winslash = "/", mustWork = FALSE)
  rootPath = normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  sub(paste0("^", rootPath, "/?"), "", absolutePath)
}

sourceFiles = sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
testFiles = sort(list.files("tests/testthat", pattern = "[.]R$", full.names = TRUE))
devFiles = character()
if (dir.exists("dev")) {
  devFiles = sort(list.files("dev", pattern = "[.](R|md)$", recursive = TRUE, full.names = TRUE))
}

sourceData = data.frame(
  File = vapply(sourceFiles, relativePath, character(1L)),
  Prefix = vapply(sourceFiles, sourcePrefix, character(1L)),
  Lines = vapply(sourceFiles, countLines, integer(1L)),
  MixedStyle = vapply(sourceFiles, isMixedStyleName, logical(1L)),
  stringsAsFactors = FALSE
)

testData = data.frame(
  File = vapply(testFiles, relativePath, character(1L)),
  Kind = vapply(testFiles, testFileKind, character(1L)),
  Lines = vapply(testFiles, countLines, integer(1L)),
  MixedStyle = vapply(testFiles, isMixedStyleName, logical(1L)),
  stringsAsFactors = FALSE
)

devData = data.frame(
  File = vapply(devFiles, relativePath, character(1L)),
  Kind = vapply(devFiles, devFileKind, character(1L)),
  Lines = vapply(devFiles, countLines, integer(1L)),
  MixedStyle = vapply(devFiles, function(path) {
    grepl("[a-z][A-Z]", basename(path))
  }, logical(1L)),
  stringsAsFactors = FALSE
)

prefixData = as.data.frame(table(sourceData$Prefix), stringsAsFactors = FALSE)
names(prefixData) = c("Prefix", "Count")
prefixData = prefixData[order(prefixData$Prefix), , drop = FALSE]

testKindData = as.data.frame(table(testData$Kind), stringsAsFactors = FALSE)
names(testKindData) = c("Kind", "Count")
testKindData = testKindData[order(testKindData$Kind), , drop = FALSE]

devKindData = as.data.frame(table(devData$Kind), stringsAsFactors = FALSE)
names(devKindData) = c("Kind", "Count")
devKindData = devKindData[order(devKindData$Kind), , drop = FALSE]

unrecognizedSourceData = sourceData[!(sourceData$Prefix %in% recognizedPrefixes) & sourceData$Prefix != "WMFM", , drop = FALSE]
mixedSourceData = sourceData[sourceData$MixedStyle, c("File", "Lines"), drop = FALSE]
mixedTestData = testData[testData$MixedStyle, c("File", "Kind", "Lines"), drop = FALSE]
helperTestData = testData[testData$Kind == "helper", c("File", "Lines", "MixedStyle"), drop = FALSE]
unexpectedTestData = testData[testData$Kind == "other", c("File", "Lines", "MixedStyle"), drop = FALSE]
activeDevData = devData[!grepl("^dev/Completed/", devData$File), c("File", "Kind", "Lines", "MixedStyle"), drop = FALSE]
completedDevData = devData[grepl("^dev/Completed/", devData$File), c("File", "Kind", "Lines", "MixedStyle"), drop = FALSE]
mixedDevData = devData[devData$MixedStyle, c("File", "Kind", "Lines"), drop = FALSE]

largestSourceData = sourceData[order(sourceData$Lines, decreasing = TRUE), c("File", "Lines"), drop = FALSE]
largestSourceData = head(largestSourceData, 20L)

smallestSourceData = sourceData[order(sourceData$Lines, sourceData$File), c("File", "Lines"), drop = FALSE]
smallestSourceData = head(smallestSourceData, 20L)

lines = c(
  "# WMFM file-organization audit report",
  "",
  paste0("Generated from: `", basename(getwd()), "`"),
  "",
  "## Source prefix counts",
  "",
  markdownTable(prefixData, c("Prefix", "Count")),
  "",
  "## Unrecognized source prefixes",
  "",
  markdownTable(unrecognizedSourceData[, c("File", "Prefix", "Lines"), drop = FALSE], c("File", "Prefix", "Lines")),
  "",
  "## Mixed-style source filenames",
  "",
  markdownTable(mixedSourceData, c("File", "Lines")),
  "",
  "## Test file kind counts",
  "",
  markdownTable(testKindData, c("Kind", "Count")),
  "",
  "## Test helper files",
  "",
  markdownTable(helperTestData, c("File", "Lines", "MixedStyle")),
  "",
  "## Unexpected testthat filenames",
  "",
  markdownTable(unexpectedTestData, c("File", "Lines", "MixedStyle")),
  "",
  "## Mixed-style test filenames",
  "",
  markdownTable(mixedTestData, c("File", "Kind", "Lines")),
  "",
  "## Developer file kind counts",
  "",
  markdownTable(devKindData, c("Kind", "Count")),
  "",
  "## Active developer files",
  "",
  markdownTable(activeDevData, c("File", "Kind", "Lines", "MixedStyle")),
  "",
  "## Mixed-style developer filenames",
  "",
  markdownTable(mixedDevData, c("File", "Kind", "Lines")),
  "",
  "## Completed developer-context files",
  "",
  markdownTable(completedDevData, c("File", "Kind", "Lines", "MixedStyle")),
  "",
  "## Largest source files",
  "",
  markdownTable(largestSourceData, c("File", "Lines")),
  "",
  "## Smallest source files",
  "",
  markdownTable(smallestSourceData, c("File", "Lines")),
  ""
)

if (nzchar(outputPath)) {
  writeLines(lines, outputPath)
} else {
  writeLines(lines)
}
