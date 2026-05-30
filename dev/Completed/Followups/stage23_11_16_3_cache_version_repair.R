#!/usr/bin/env Rscript

# Stage 23.11.16.3 cache version repair
#
# This script is intentionally narrow. It repairs the cache-version token
# accidentally advanced during the Stage 23.11 follow-up work, without changing
# follow-up parsing, prompt construction, or deterministic prediction logic.

message("Running Stage 23.11.16.3 cache version repair")

oldToken = "stage23.11.17-v1"
newToken = "stage20.13-v1"

candidateRoots = c("R", file.path("tests", "testthat"))
existingRoots = candidateRoots[dir.exists(candidateRoots)]

if (length(existingRoots) == 0) {
  stop("No R or tests/testthat directories were found. Run this from the WMFM package root.", call. = FALSE)
}

candidateFiles = unlist(
  lapply(
    existingRoots,
    function(root) {
      list.files(
        root,
        pattern = "\\.[Rr]$",
        recursive = TRUE,
        full.names = TRUE
      )
    }
  ),
  use.names = FALSE
)

if (length(candidateFiles) == 0) {
  stop("No R source or test files were found under R or tests/testthat.", call. = FALSE)
}

changedFiles = character(0)
matchedFiles = character(0)

for (filePath in candidateFiles) {
  lines = readLines(filePath, warn = FALSE)

  if (!any(grepl(oldToken, lines, fixed = TRUE))) {
    next
  }

  matchedFiles = c(matchedFiles, filePath)
  repairedLines = gsub(oldToken, newToken, lines, fixed = TRUE)

  if (!identical(lines, repairedLines)) {
    writeLines(repairedLines, filePath, useBytes = TRUE)
    changedFiles = c(changedFiles, filePath)
  }
}

if (length(matchedFiles) == 0) {
  message("No files contained the unexpected cache-version token: ", oldToken)
  message("No source files were changed by this repair script.")
} else {
  message("Found unexpected cache-version token in:")
  for (filePath in matchedFiles) {
    message("- ", filePath)
  }
}

if (length(changedFiles) > 0) {
  message("Updated cache-version token to ", newToken, " in:")
  for (filePath in changedFiles) {
    message("- ", filePath)
  }
} else {
  message("No file content changes were needed.")
}
