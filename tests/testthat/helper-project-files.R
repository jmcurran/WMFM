findWmfmProjectFile = function(...) {
  relativePath = file.path(...)

  candidatePaths = c(
    file.path(testthat::test_path("..", ".."), relativePath),
    file.path(getwd(), "..", "..", relativePath),
    file.path(getwd(), "..", "00_pkg_src", "WMFM", relativePath),
    file.path(getwd(), "..", "..", "00_pkg_src", "WMFM", relativePath)
  )

  currentPath = normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(8)) {
    candidatePaths = c(candidatePaths, file.path(currentPath, relativePath))
    descriptionPath = file.path(currentPath, "DESCRIPTION")
    if (file.exists(descriptionPath)) {
      descriptionLines = readLines(descriptionPath, warn = FALSE)
      if (any(grepl("^Package:[[:space:]]*WMFM[[:space:]]*$", descriptionLines))) {
        candidatePaths = c(candidatePaths, file.path(currentPath, relativePath))
      }
    }
    parentPath = dirname(currentPath)
    if (identical(parentPath, currentPath)) {
      break
    }
    currentPath = parentPath
  }

  candidatePaths = unique(normalizePath(candidatePaths, winslash = "/", mustWork = FALSE))
  existingPaths = candidatePaths[file.exists(candidatePaths)]

  if (length(existingPaths) < 1) {
    testthat::skip(paste("Source-tree file not available during this test run:", relativePath))
  }

  existingPaths[[1]]
}
