test_that("provider settings status lines use concise active-profile format and hide secrets", {
  tmpDir = tempfile("wmfm-provider-settings-")
  withr::local_options(list(wmfm.config_dir = tmpDir))
  withr::local_envvar(list(ANTHROPIC_API_KEY = "super-secret-value"), .local_envir = parent.frame())

  statusLines = buildProviderSettingsStatusLines(buildProviderSettingsState())
  statusText = paste(statusLines, collapse = "\n")

  expect_match(statusText, "Active provider:", fixed = TRUE)
  expect_match(statusText, "Provider type:", fixed = TRUE)
  expect_match(statusText, "Model:", fixed = TRUE)
  expect_match(statusText, "Credential:", fixed = TRUE)
  expect_match(statusText, "API key values are never stored or displayed by WMFM.", fixed = TRUE)
  expect_false(grepl("super-secret-value", statusText, fixed = TRUE))
})

test_that("non-secret provider config preparation and persistence exclude key-like fields", {
  tmpDir = tempfile("wmfm-provider-settings-write-")
  withr::local_options(list(wmfm.config_dir = tmpDir))

  prepared = prepareNonSecretProviderConfig(
    backend = " claude ",
    ollamaBaseUrl = " http://localhost:11434 ",
    ollamaModel = " llama3.2 ",
    ollamaThinkLow = TRUE
  )

  expect_identical(prepared$backend, "claude")
  expect_identical(prepared$ollamaBaseUrl, "http://localhost:11434")
  expect_identical(prepared$ollamaModel, "llama3.2")
  expect_true(prepared$ollamaThinkLow)

  saveNonSecretProviderConfig(c(
    prepared,
    list(
      anthropicApiKey = "never-save-me",
      ANTHROPIC_API_KEY = "never-save-me-either",
      apiKey = "still-never-save"
    )
  ))

  persisted = readWmfmConfig()
  expect_identical(persisted$backend, "claude")
  expect_identical(persisted$ollamaBaseUrl, "http://localhost:11434")
  expect_identical(persisted$ollamaModel, "llama3.2")
  expect_true(persisted$ollamaThinkLow)
  expect_false(any(grepl("key", names(persisted), ignore.case = TRUE)))
})

test_that("provider config persistence preserves developer-mode preference", {
  tmpDir = tempfile("wmfm-provider-settings-developer-mode-")
  withr::local_options(list(wmfm.config_dir = tmpDir))

  saveDeveloperModePreference(TRUE)
  saveNonSecretProviderConfig(list(
    backend = "claude",
    ollamaBaseUrl = "http://custom",
    ollamaModel = "custom-model",
    ollamaThinkLow = FALSE
  ))

  persisted = readWmfmConfig()
  expect_identical(persisted$backend, "claude")
  expect_true(isTRUE(persisted$developerModeEnabled))
})

test_that("resetNonSecretProviderConfig restores defaults", {
  tmpDir = tempfile("wmfm-provider-settings-reset-")
  withr::local_options(list(wmfm.config_dir = tmpDir))

  saveNonSecretProviderConfig(list(
    backend = "claude",
    ollamaBaseUrl = "http://custom",
    ollamaModel = "custom-model",
    ollamaThinkLow = TRUE
  ))

  saveDeveloperModePreference(TRUE)
  resetNonSecretProviderConfig()

  persisted = readWmfmConfig()
  expect_identical(persisted$backend, wmfmProviderDefaults()$backend)
  expect_identical(persisted$ollamaModel, wmfmProviderDefaults()$ollamaModel)
  expect_true(isTRUE(persisted$developerModeEnabled))
})

test_that("provider settings labels keep Ollama-specific controls explicit", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "Ollama-specific and apply only when Ollama is selected", fixed = TRUE)
  expect_match(uiText, "Ollama base URL (Ollama only)", fixed = TRUE)
  expect_match(uiText, "Ollama model (Ollama only)", fixed = TRUE)
  expect_match(uiText, "Refresh available Ollama models", fixed = TRUE)
  expect_match(uiText, "Default to low thinking for Ollama (Ollama only)", fixed = TRUE)
})


test_that("Claude credential guidance references ANTHROPIC_API_KEY without revealing values", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "very-secret-token"), .local_envir = parent.frame())

  guidance = paste(buildProviderCredentialGuidance("claude"), collapse = "\n")
  statusText = paste(buildProviderCredentialStatusLines("claude"), collapse = "\n")

  expect_match(guidance, "ANTHROPIC_API_KEY", fixed = TRUE)
  expect_match(statusText, "ANTHROPIC_API_KEY", fixed = TRUE)
  expect_false(grepl("very-secret-token", guidance, fixed = TRUE))
  expect_false(grepl("very-secret-token", statusText, fixed = TRUE))
})

test_that("missing Claude credentials produce clear missing status", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = ""), .local_envir = parent.frame())

  statusText = paste(buildProviderCredentialStatusLines("claude"), collapse = "\n")

  expect_match(statusText, "Credential status: missing", fixed = TRUE)
  expect_match(statusText, "environment variable ANTHROPIC_API_KEY", fixed = TRUE)
})

test_that("Ollama guidance clearly states no API key is required", {
  guidance = paste(buildProviderCredentialGuidance("ollama"), collapse = "\n")

  expect_match(guidance, "does not require an API key", fixed = TRUE)
  expect_match(guidance, "reachable Ollama base URL", fixed = TRUE)
})

test_that("provider settings UI no longer includes provider switch password input", {
  uiText = readPackageText("R", "app-ui.R")

  expect_false(grepl("providerSwitchPassword", uiText, fixed = TRUE))
  expect_false(grepl("Password required to switch/save Claude", uiText, fixed = TRUE))
})


test_that("provider settings UI is consolidated to one heading", {
  html = as.character(appUI())

  expect_match(html, "Provider settings", fixed = TRUE)
  expect_no_match(html, "Chat provider", fixed = TRUE)
  expect_no_match(html, "Provider config (local, non-secret)", fixed = TRUE)
})

test_that("explanation provenance text includes provider and hh:mm:ss time", {
  txt = buildExplanationProvenanceText(
    providerLabel = "Ollama",
    modelName = "gpt-oss",
    generatedAt = as.POSIXct("2026-05-22 09:10:11", tz = "UTC")
  )

  expect_identical(txt, "Explanation generated by Ollama (gpt-oss) at 09:10:11")
})

test_that("optional follow-up placeholder is neutral and visually subdued", {
  uiText = readPackageText("R", "app-ui.R")

  expect_match(uiText, "Optional: ask a follow-up question about this fitted model.", fixed = TRUE)
  expect_match(uiText, "#modelFollowupQuestion::placeholder", fixed = TRUE)
  expect_match(uiText, "color: #9aa0a6", fixed = TRUE)
})

test_that("explanation prompt diagnostics rely on accordion title styling", {
  uiText = readPackageText("R", "app-explanation-diagnostics-ui.R")

  expect_false(grepl('tags$strong("Explanation prompt diagnostics")', uiText, fixed = TRUE))
  expect_match(uiText, "Explanation prompt diagnostics", fixed = TRUE)
  expect_match(uiText, "wmfm-explanation-helper-box", fixed = TRUE)
})
