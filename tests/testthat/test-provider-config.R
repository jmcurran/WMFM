test_that("wmfmProviderDefaults exposes current package defaults", {
  defaults = wmfmProviderDefaults()

  expect_identical(defaults$backend, "ollama")
  expect_identical(defaults$ollamaBaseUrl, "http://corrin.stat.auckland.ac.nz:11434")
  expect_identical(defaults$ollamaModel, "gpt-oss")
  expect_identical(defaults$ollamaThinkLow, FALSE)
})

test_that("resolveWmfmProviderConfig uses options and normalises blanks", {
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


test_that("hasClaudeApiKey reflects ANTHROPIC_API_KEY availability", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")
  expect_false(hasClaudeApiKey())

  withr::local_envvar(ANTHROPIC_API_KEY = "abc123")
  expect_true(hasClaudeApiKey())
})
