#!/usr/bin/env Rscript

message("Running CRAN reviewer issue audit")

existingPaths = function(paths) {
  paths[file.exists(paths)]
}

listFiles = function(paths, pattern = NULL) {
  paths = existingPaths(paths)
  if (length(paths) == 0L) {
    return(character())
  }
  unlist(
    lapply(
      paths,
      function(path) {
        list.files(path, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE, pattern = pattern)
      }
    ),
    use.names = FALSE
  )
}

readFileLines = function(file) {
  readLines(file, warn = FALSE)
}

findPattern = function(files, pattern, fixed = FALSE) {
  hits = list()
  for (file in files) {
    lines = readFileLines(file)
    matched = grep(pattern, lines, fixed = fixed, perl = !fixed)
    if (length(matched) > 0L) {
      hits[[file]] = data.frame(
        file = file,
        line = matched,
        text = lines[matched],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(hits) == 0L) {
    return(data.frame(file = character(), line = integer(), text = character()))
  }
  do.call(rbind, hits)
}

printHits = function(label, hits) {
  if (nrow(hits) == 0L) {
    message("OK: ", label)
    return(invisible(TRUE))
  }

  message("FAILED: ", label)
  for (idx in seq_len(nrow(hits))) {
    message(hits$file[idx], ":", hits$line[idx], ": ", hits$text[idx])
  }
  invisible(FALSE)
}

rFiles = listFiles("R", "[.]R$")
manFiles = listFiles("man", "[.]Rd$")
testFiles = listFiles("tests", "[.]R$")
installedFiles = c(rFiles, testFiles, listFiles(c("inst", "vignettes", "examples")))

failures = 0L

checks = list(
  list(
    label = "no triple-colon usage in package code, tests, or generated documentation",
    hits = findPattern(c(rFiles, testFiles, manFiles), ":::", fixed = TRUE)
  ),
  list(
    label = "no dontrun blocks in package code or generated documentation",
    hits = findPattern(c(rFiles, manFiles), paste0("\\", "dontrun"), fixed = TRUE)
  ),
  list(
    label = "no installed.packages() calls in package code or tests",
    hits = findPattern(c(rFiles, testFiles), "installed[.]packages[[:space:]]*\\(", fixed = FALSE)
  ),
  list(
    label = "no global environment writes or globalenv() lookups in package code or tests",
    hits = findPattern(c(rFiles, testFiles), "[.]GlobalEnv|globalenv[[:space:]]*\\(", fixed = FALSE)
  )
)

for (check in checks) {
  ok = printHits(check$label, check$hits)
  if (!ok) {
    failures = failures + 1L
  }
}

writePatterns = c(
  "writeLines[[:space:]]*\\(",
  "write_json[[:space:]]*\\(",
  "file[.]create[[:space:]]*\\(",
  "dir[.]create[[:space:]]*\\(",
  "saveRDS[[:space:]]*\\(",
  "write[.]csv[[:space:]]*\\(",
  "write[.]table[[:space:]]*\\("
)
writeHits = findPattern(c(rFiles, testFiles), paste(writePatterns, collapse = "|"), fixed = FALSE)
if (nrow(writeHits) > 0L) {
  message("INFO: file-writing calls found; review that package code writes only after explicit user action and tests use temporary paths.")
  for (idx in seq_len(nrow(writeHits))) {
    message(writeHits$file[idx], ":", writeHits$line[idx], ": ", writeHits$text[idx])
  }
} else {
  message("OK: no file-writing calls found")
}

if (failures > 0L) {
  stop("CRAN reviewer issue audit failed", call. = FALSE)
}

message("CRAN reviewer issue audit passed")
