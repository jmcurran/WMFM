findPackageRoot = function(startDir = getwd()) {
  startDir = normalizePath(startDir, winslash = "/", mustWork = TRUE)

  candidates = unique(c(
    startDir,
    dirname(startDir),
    dirname(dirname(startDir)),
    dirname(dirname(dirname(startDir))),
    Sys.getenv("TESTTHAT_PKG_PATH", unset = "")
  ))

  candidates = candidates[nzchar(candidates)]

  for (candidate in candidates) {
    descriptionPath = file.path(candidate, "DESCRIPTION")
    rPath = file.path(candidate, "R")

    if (file.exists(descriptionPath) && dir.exists(rPath)) {
      return(candidate)
    }
  }

  NA_character_
}

readPackageText = function(...) {
  packageRoot = findPackageRoot()

  if (is.na(packageRoot)) {
    testthat::skip("Package source tree is not available in this test environment")
  }

  filePath = file.path(packageRoot, ...)

  if (!file.exists(filePath)) {
    testthat::skip(paste0(
      "Package source file is not available in this test environment: ",
      paste(..., collapse = "/")
    ))
  }

  paste(readLines(filePath, warn = FALSE), collapse = "\n")
}

test_that("settings page includes an Ollama low-thinking switch", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "checkboxInput\\(\\s*inputId = \"ollama_think_low\"", perl = TRUE)
  expect_match(uiText, "Use low thinking effort for Ollama", fixed = TRUE)
  expect_match(uiText, "think = \\\"low\\\"", fixed = TRUE)
})

test_that("app server passes the low-thinking setting to the chat provider", {
  serverText = readPackageText("R", "app-server.R")

  expect_match(serverText, "activeOllamaThinkLow = FALSE", fixed = TRUE)
  expect_match(serverText, "rv\\$activeOllamaThinkLow = isTRUE\\(input\\$ollama_think_low\\)", perl = TRUE)
  expect_match(serverText, "ollamaThinkLow = rv\\$activeOllamaThinkLow %\\|\\|% FALSE", perl = TRUE)
})

test_that("getChatProvider sends think low through ellmer params", {
  utilsText = readPackageText("R", "utils-llm.R")

  expect_match(utilsText, "ollamaThinkLow", fixed = TRUE)
  expect_match(utilsText, "params\\(think = \"low\"\\)", perl = TRUE)
  expect_match(utilsText, "do.call\\(chat_ollama, ollamaArgs\\)", perl = TRUE)
})
