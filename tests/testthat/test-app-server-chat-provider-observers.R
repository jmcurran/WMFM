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

test_that("chat provider observer registration is extracted from app server", {
  appServerText = readPackageText("R", "app-server.R")
  chatProviderText = readPackageText("R", "app-server-chat-provider.R")

  expect_match(appServerText, "registerChatProviderObservers", fixed = TRUE)
  expect_false(grepl("refreshOllamaModelChoices = function", appServerText, fixed = TRUE))

  expect_match(chatProviderText, "registerChatProviderObservers = function", fixed = TRUE)
  expect_match(chatProviderText, "refreshOllamaModelChoices = function", fixed = TRUE)
  expect_match(chatProviderText, "verifyProviderSwitchPassword", fixed = TRUE)
  expect_match(chatProviderText, "buildChatProviderSetMessage", fixed = TRUE)
})
