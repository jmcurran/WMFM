test_that("provider settings status lines are safe and include config metadata", {
  tmpDir = tempfile("wmfm-provider-settings-")
  withr::local_options(list(wmfm.config_dir = tmpDir))
  withr::local_envvar(list(ANTHROPIC_API_KEY = "super-secret-value"), .local_envir = parent.frame())

  statusLines = buildProviderSettingsStatusLines(buildProviderSettingsState())
  statusText = paste(statusLines, collapse = "\n")

  expect_match(statusText, "Config file path:", fixed = TRUE)
  expect_match(statusText, "Config file exists:", fixed = TRUE)
  expect_match(statusText, "Config file readable:", fixed = TRUE)
  expect_match(statusText, "Custom config directory option active:", fixed = TRUE)
  expect_match(statusText, "Configured provider/backend:", fixed = TRUE)
  expect_match(statusText, "source: env:ANTHROPIC_API_KEY", fixed = TRUE)
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
