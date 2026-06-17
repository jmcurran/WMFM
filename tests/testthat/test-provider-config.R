test_that("wmfmProviderDefaults exposes current package defaults", {
  defaults = wmfmProviderDefaults()

  expect_identical(defaults$backend, "ollama")
  expect_identical(defaults$ollamaBaseUrl, "")
  expect_identical(defaults$ollamaModel, "gpt-oss")
  expect_identical(defaults$ollamaThinkLow, FALSE)
})

test_that("resolveWmfmProviderConfig uses options and normalises blanks", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  withr::local_options(list(
    wmfm.chat_backend = " CLAUDE ",
    wmfm.ollama_base_url = " http://localhost:11434 ",
    wmfm.ollama_model = " llama3.2 ",
    wmfm.ollama_think_low = TRUE
  ))

  cfg = resolveWmfmProviderConfig()

  expect_identical(cfg$backend, "claude")
  expect_identical(cfg$ollamaBaseUrl, "http://localhost:11434")
  expect_identical(cfg$ollamaModel, "llama3.2")
  expect_identical(cfg$ollamaThinkLow, TRUE)

  withr::local_options(list(
    wmfm.chat_backend = "",
    wmfm.ollama_base_url = "",
    wmfm.ollama_model = "",
    wmfm.ollama_think_low = FALSE
  ))

  cfgBlank = resolveWmfmProviderConfig()
  defaults = wmfmProviderDefaults()

  expect_identical(cfgBlank$backend, defaults$backend)
  expect_identical(cfgBlank$ollamaBaseUrl, defaults$ollamaBaseUrl)
  expect_identical(cfgBlank$ollamaModel, defaults$ollamaModel)
  expect_identical(cfgBlank$ollamaThinkLow, FALSE)
})

test_that("resolveWmfmProviderConfig explicit overrides options", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  withr::local_options(list(
    wmfm.chat_backend = "ollama",
    wmfm.ollama_base_url = "http://a",
    wmfm.ollama_model = "m1",
    wmfm.ollama_think_low = FALSE
  ))

  cfg = resolveWmfmProviderConfig(
    backend = " Claude ",
    ollamaBaseUrl = " http://b ",
    ollamaModel = " m2 ",
    ollamaThinkLow = TRUE
  )

  expect_identical(cfg$backend, "claude")
  expect_identical(cfg$ollamaBaseUrl, "http://b")
  expect_identical(cfg$ollamaModel, "m2")
  expect_identical(cfg$ollamaThinkLow, TRUE)
})

test_that("wmfmConfigPath resolves via temporary config directory", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  expect_identical(wmfmConfigDir(), tmpDir)
  expect_identical(wmfmConfigPath(), file.path(tmpDir, "config.json"))
})

test_that("readWmfmConfig returns empty list when config file is missing", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  expect_false(file.exists(wmfmConfigPath()))
  expect_identical(readWmfmConfig(), list())
})

test_that("writeWmfmConfig and readWmfmConfig round-trip non-secret fields", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  cfgIn = list(
    backend = "claude",
    ollamaBaseUrl = "http://localhost:11434",
    ollamaModel = "llama3.2",
    ollamaThinkLow = TRUE
  )
  writeWmfmConfig(cfgIn)

  expect_true(file.exists(wmfmConfigPath()))
  expect_identical(readWmfmConfig(), cfgIn)
})

test_that("readWmfmConfig ignores unknown fields consistently", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  jsonlite::write_json(
    list(
      backend = "ollama",
      unknownField = "surprise"
    ),
    path = wmfmConfigPath(),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  expect_identical(readWmfmConfig(), list(backend = "ollama"))
})

test_that("writeWmfmConfig does not persist API keys", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  writeWmfmConfig(list(
    backend = "claude",
    ANTHROPIC_API_KEY = "secret",
    apiKey = "secret2",
    anthropicApiKey = "secret3"
  ))

  rawJson = paste(readLines(wmfmConfigPath(), warn = FALSE), collapse = "\n")
  expect_false(grepl("ANTHROPIC_API_KEY", rawJson, fixed = TRUE))
  expect_false(grepl("apiKey", rawJson, fixed = TRUE))
  expect_false(grepl("anthropicApiKey", rawJson, fixed = TRUE))
  expect_identical(readWmfmConfig(), list(backend = "claude"))
})

test_that("resolver precedence is explicit overrides then options then local config then defaults", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  writeWmfmConfig(list(
    backend = "claude",
    ollamaBaseUrl = "http://persisted",
    ollamaModel = "persisted-model",
    ollamaThinkLow = TRUE
  ))

  cfgFromLocal = resolveWmfmProviderConfig()
  expect_identical(cfgFromLocal$backend, "claude")
  expect_identical(cfgFromLocal$ollamaBaseUrl, "http://persisted")
  expect_identical(cfgFromLocal$ollamaModel, "persisted-model")
  expect_identical(cfgFromLocal$ollamaThinkLow, TRUE)

  withr::local_options(list(
    wmfm.chat_backend = "ollama",
    wmfm.ollama_base_url = "http://option",
    wmfm.ollama_model = "option-model",
    wmfm.ollama_think_low = FALSE
  ))

  cfgFromOptions = resolveWmfmProviderConfig()
  expect_identical(cfgFromOptions$backend, "ollama")
  expect_identical(cfgFromOptions$ollamaBaseUrl, "http://option")
  expect_identical(cfgFromOptions$ollamaModel, "option-model")
  expect_identical(cfgFromOptions$ollamaThinkLow, FALSE)

  cfgExplicit = resolveWmfmProviderConfig(
    backend = " claude ",
    ollamaBaseUrl = " http://explicit ",
    ollamaModel = " explicit-model ",
    ollamaThinkLow = TRUE
  )
  expect_identical(cfgExplicit$backend, "claude")
  expect_identical(cfgExplicit$ollamaBaseUrl, "http://explicit")
  expect_identical(cfgExplicit$ollamaModel, "explicit-model")
  expect_identical(cfgExplicit$ollamaThinkLow, TRUE)
})


test_that("hasClaudeApiKey reflects ANTHROPIC_API_KEY availability", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")
  expect_false(hasClaudeApiKey())

  withr::local_envvar(ANTHROPIC_API_KEY = "abc123")
  expect_true(hasClaudeApiKey())
})

test_that("resolveWmfmProviderCredentials reports Ollama as no-credential local provider", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")

  credentials = resolveWmfmProviderCredentials()
  expect_false(credentials$ollama$requiresCredentials)
  expect_true(credentials$ollama$credentialsAvailable)
  expect_identical(credentials$ollama$credentialSource, "none-required")
  expect_true(credentials$ollama$localOnly)
})

test_that("provider adapter registry includes supported runtime providers", {
  registry = wmfmProviderRegistry()

  expect_true("ollama" %in% names(registry))
  expect_true("claude" %in% names(registry))
  expect_identical(registry$ollama$adapterKind, "chat_ollama")
  expect_identical(registry$claude$adapterKind, "chat_anthropic")
})

test_that("provider adapter metadata reports credential requirements correctly", {
  ollamaAdapter = getWmfmProviderAdapter("ollama")
  claudeAdapter = getWmfmProviderAdapter("claude")

  expect_false(ollamaAdapter$requiresCredentials)
  expect_true(claudeAdapter$requiresCredentials)
  expect_identical(claudeAdapter$credentialEnvVar, "ANTHROPIC_API_KEY")
})

test_that("unsupported provider adapter lookup fails clearly", {
  expect_error(
    getWmfmProviderAdapter("not-a-provider"),
    "Unsupported chat backend: not-a-provider",
    fixed = TRUE
  )
  expect_false(isWmfmProviderSupported("not-a-provider"))
})

test_that("Claude credentials are reported as missing when ANTHROPIC_API_KEY is unset", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")

  credentials = resolveWmfmProviderCredentials()
  expect_true(credentials$claude$requiresCredentials)
  expect_false(credentials$claude$credentialsAvailable)
  expect_identical(credentials$claude$credentialSource, "missing")
  expect_false(hasWmfmProviderCredentials("claude"))
})

test_that("Claude credentials are reported as available when ANTHROPIC_API_KEY is set", {
  withr::local_envvar(ANTHROPIC_API_KEY = "super-secret-token")

  credentials = resolveWmfmProviderCredentials()
  expect_true(credentials$claude$credentialsAvailable)
  expect_identical(credentials$claude$credentialSource, "env:ANTHROPIC_API_KEY")
  expect_true(hasWmfmProviderCredentials("claude"))
})

test_that("provider status does not expose secret values", {
  secretValue = "top-secret-provider-key"
  withr::local_envvar(ANTHROPIC_API_KEY = secretValue)

  status = describeWmfmProviderStatus()
  statusText = paste(capture.output(str(status)), collapse = "\n")

  expect_false(grepl(secretValue, statusText, fixed = TRUE))
  expect_true(grepl("env:ANTHROPIC_API_KEY", statusText, fixed = TRUE))
})

test_that("provider status helper remains consistent with provider registry metadata", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")

  status = describeWmfmProviderStatus()
  registry = wmfmProviderRegistry()

  expect_identical(
    status$providers$ollama$requiresCredentials,
    registry$ollama$requiresCredentials
  )
  expect_identical(
    status$providers$claude$requiresCredentials,
    registry$claude$requiresCredentials
  )
  expect_identical(
    status$providers$openai$requiresCredentials,
    registry$openai$requiresCredentials
  )
  expect_identical(
    status$providers$openaiCompatible$requiresCredentials,
    registry$openaiCompatible$requiresCredentials
  )
})

test_that("describeWmfmConfigLocation reports temporary config paths", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  detailsMissing = describeWmfmConfigLocation()
  expect_identical(detailsMissing$configPath, file.path(tmpDir, "config.json"))
  expect_false(detailsMissing$exists)
  expect_false(detailsMissing$readable)
  expect_true(detailsMissing$customConfigDirActive)

  writeWmfmConfig(list(backend = "ollama"))
  detailsPresent = describeWmfmConfigLocation()
  expect_true(detailsPresent$exists)
  expect_true(detailsPresent$readable)
})

test_that("provider status and config persistence preserve stage 22 precedence behavior", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))
  writeWmfmConfig(list(
    backend = "claude",
    ollamaBaseUrl = "http://persisted-host",
    ollamaModel = "persisted-model",
    ollamaThinkLow = TRUE
  ))

  withr::local_options(list(
    wmfm.chat_backend = "ollama",
    wmfm.ollama_base_url = "http://option-host",
    wmfm.ollama_model = "option-model",
    wmfm.ollama_think_low = FALSE
  ))

  resolved = resolveWmfmProviderConfig(
    backend = " claude ",
    ollamaBaseUrl = " http://explicit-host ",
    ollamaModel = " explicit-model ",
    ollamaThinkLow = TRUE
  )

  expect_identical(resolved$backend, "claude")
  expect_identical(resolved$ollamaBaseUrl, "http://explicit-host")
  expect_identical(resolved$ollamaModel, "explicit-model")
  expect_identical(resolved$ollamaThinkLow, TRUE)

  rawJson = paste(readLines(wmfmConfigPath(), warn = FALSE), collapse = "\n")
  expect_false(grepl("ANTHROPIC_API_KEY", rawJson, fixed = TRUE))
})

test_that("provider profiles default and legacy migration are available", {
  withr::local_options(list(wmfm.config_dir = tempfile("wmfm-profiles-")))
  profiles = readWmfmProviderProfiles()
  expect_true(length(profiles) >= 2)
  expect_true(any(vapply(profiles, function(x) identical(x$providerType, "ollama"), logical(1))))
  expect_true(any(vapply(profiles, function(x) identical(x$providerType, "claude"), logical(1))))

  legacy = migrateLegacyProviderConfigToProfiles(list(backend = "claude", ollamaBaseUrl = "http://x", ollamaModel = "m"))
  active = resolveWmfmActiveProviderProfile("claude")
  expect_true(any(vapply(legacy, function(x) isTRUE(x$active), logical(1))))
  expect_identical(active$providerType, "claude")
})

test_that("provider profiles round-trip metadata without secret values", {
  withr::local_options(list(wmfm.config_dir = tempfile("wmfm-profiles-write-")))
  profiles = list(
    list(profileId = "openai-main", displayName = "OpenAI Main", providerType = "openai", credentialSource = "envvar", credentialEnvVar = "OPENAI_API_KEY", defaultModel = "gpt-4.1"),
    list(profileId = "openai-compatible-local", displayName = "Local Compatible", providerType = "openaiCompatible", apiUrl = "http://localhost:11434/v1", credentialSource = "envvar", credentialEnvVar = "OPENAI_API_KEY", apiKey = "must-not-save")
  )
  writeWmfmProviderProfiles(profiles)
  parsed = readWmfmProviderProfiles()
  expect_identical(parsed[[1]]$providerType, "openai")
  expect_identical(parsed[[2]]$providerType, "openaiCompatible")
  raw = paste(readLines(wmfmConfigPath(), warn = FALSE), collapse = "\n")
  expect_false(grepl("must-not-save", raw, fixed = TRUE))
})


test_that("developer mode is opt-in by environment and persisted when exposed", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))
  withr::local_envvar(list(WMFM_SHOW_DEVELOPER_MODE = ""), .local_envir = parent.frame())

  expect_false(isDeveloperModeUiEnabled())
  expect_false(resolveDeveloperModePreference())

  saveDeveloperModePreference(TRUE)
  expect_false(resolveDeveloperModePreference())
  expect_true(isTRUE(readWmfmConfig()$developerModeEnabled))

  withr::local_envvar(list(WMFM_SHOW_DEVELOPER_MODE = "1"), .local_envir = parent.frame())
  expect_true(isDeveloperModeUiEnabled())
  expect_true(resolveDeveloperModePreference())
})

test_that("writeWmfmConfig preserves provider and developer-mode preferences", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  writeWmfmConfig(list(
    backend = "ollama",
    ollamaBaseUrl = "http://localhost:11434",
    ollamaModel = "manual-model",
    ollamaThinkLow = TRUE,
    developerModeEnabled = TRUE,
    apiKey = "do-not-save"
  ))

  persisted = readWmfmConfig()

  expect_identical(persisted$backend, "ollama")
  expect_identical(persisted$ollamaModel, "manual-model")
  expect_true(persisted$ollamaThinkLow)
  expect_true(isTRUE(persisted$developerModeEnabled))
  expect_null(persisted$apiKey)
})


test_that("startup provider readiness requires usable credentials or explicit Ollama config", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))
  withr::local_envvar(list(ANTHROPIC_API_KEY = ""), .local_envir = parent.frame())

  expect_false(hasExplicitWmfmProviderConfig())
  expect_false(isWmfmProviderReadyForStartup())
  expect_match(buildMissingProviderStartupMessage(), "README", fixed = TRUE)

  withr::local_envvar(list(ANTHROPIC_API_KEY = "secret"), .local_envir = parent.frame())
  expect_identical(resolveWmfmProviderConfig()$backend, wmfmProviderDefaults()$backend)
  expect_false(isWmfmProviderReadyForStartup())

  writeWmfmConfig(list(backend = "claude"))
  expect_true(isWmfmProviderReadyForStartup())

  withr::local_envvar(list(ANTHROPIC_API_KEY = ""), .local_envir = parent.frame())
  saveNonSecretProviderConfig(list(
    backend = "ollama",
    ollamaBaseUrl = "http://localhost:11434",
    ollamaModel = "llama3.1",
    ollamaThinkLow = FALSE
  ))
  expect_true(hasExplicitWmfmProviderConfig())
  expect_true(isWmfmProviderReadyForStartup())
})


test_that("developer mode preference persists only when developer UI is exposed", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))

  withr::local_envvar(WMFM_SHOW_DEVELOPER_MODE = "")
  saveDeveloperModePreference(TRUE)
  expect_false(resolveDeveloperModePreference())
  expect_true(isTRUE(readWmfmConfig()$developerModeEnabled))

  withr::local_envvar(WMFM_SHOW_DEVELOPER_MODE = "1")
  expect_true(resolveDeveloperModePreference())

  saveDeveloperModePreference(FALSE)
  expect_false(resolveDeveloperModePreference())
  expect_false(isTRUE(readWmfmConfig()$developerModeEnabled))
})

test_that("provider resolver does not infer Claude solely from API-key presence", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir))
  withr::local_envvar(ANTHROPIC_API_KEY = "secret-token")

  cfg = resolveWmfmProviderConfig()

  expect_identical(cfg$backend, wmfmProviderDefaults()$backend)
})

test_that("local config credentials are available for desktop sessions without readWmfmConfig exposing values", {
  tmpDir = tempfile("wmfm-config-credentials-")
  withr::local_options(list(wmfm.config_dir = tmpDir, wmfm.deployed_app = FALSE))
  withr::local_envvar(list(
    WMFM_ALLOW_CONFIG_CREDENTIALS = "1",
    ANTHROPIC_API_KEY = ""
  ), .local_envir = parent.frame())

  writeWmfmConfigCredential("claude", "local-secret-value")

  credential = resolveWmfmProviderCredential("claude")
  credentials = resolveWmfmProviderCredentials()
  publicConfig = readWmfmConfig()
  rawConfig = readWmfmRawConfig()

  expect_true(credential$available)
  expect_identical(credential$source, "wmfm-config")
  expect_true(credentials$claude$credentialsAvailable)
  expect_identical(credentials$claude$credentialSource, "wmfm-config")
  expect_false("credentials" %in% names(publicConfig))
  expect_identical(rawConfig$credentials$claude$apiKey, "local-secret-value")
})

test_that("environment credentials take precedence over local config credentials", {
  tmpDir = tempfile("wmfm-config-credential-precedence-")
  withr::local_options(list(wmfm.config_dir = tmpDir, wmfm.deployed_app = FALSE))
  withr::local_envvar(list(
    WMFM_ALLOW_CONFIG_CREDENTIALS = "1",
    ANTHROPIC_API_KEY = "env-secret-value"
  ), .local_envir = parent.frame())

  writeWmfmConfigCredential("claude", "local-secret-value")

  credential = resolveWmfmProviderCredential("claude")

  expect_true(credential$available)
  expect_identical(credential$source, "env:ANTHROPIC_API_KEY")
})

test_that("deployed sessions cannot use local config credential storage", {
  tmpDir = tempfile("wmfm-config-credential-deployed-")
  withr::local_options(list(wmfm.config_dir = tmpDir, wmfm.deployed_app = TRUE))
  withr::local_envvar(list(
    WMFM_ALLOW_CONFIG_CREDENTIALS = "1",
    ANTHROPIC_API_KEY = ""
  ), .local_envir = parent.frame())

  expect_false(isWmfmConfigCredentialStorageAllowed())
  expect_error(
    writeWmfmConfigCredential("claude", "local-secret-value"),
    "not allowed",
    fixed = TRUE
  )
})


test_that("provider registry rows expose user-facing status without secrets", {
  withr::local_tempdir() -> tmpDir
  withr::local_options(list(wmfm.config_dir = tmpDir, wmfm.deployed_app = FALSE))
  withr::local_envvar(list(ANTHROPIC_API_KEY = ""), .local_envir = parent.frame())

  rows = buildWmfmProviderRegistryRows(list(
    list(displayName = "Local Ollama", providerType = "ollama", apiUrl = "", defaultModel = "llama3"),
    list(displayName = "Claude", providerType = "claude", credentialEnvVar = "ANTHROPIC_API_KEY")
  ))

  expect_identical(names(rows), c("Name", "Type", "Status"))
  expect_true("Setup needed" %in% rows$Status)
  expect_true("Credential needed" %in% rows$Status)
  expect_false(any(grepl("ANTHROPIC_API_KEY", rows$Status, fixed = TRUE)))
})
