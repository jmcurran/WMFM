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
  expect_identical(names(persisted), c("backend", "ollamaBaseUrl", "ollamaModel", "ollamaThinkLow"))
  expect_false(any(grepl("key", names(persisted), ignore.case = TRUE)))
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

  resetNonSecretProviderConfig()
  expect_identical(readWmfmConfig(), wmfmProviderDefaults())
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
