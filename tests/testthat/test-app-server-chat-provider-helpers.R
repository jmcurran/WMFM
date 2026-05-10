test_that("chat provider status helper reports Claude", {
  expect_equal(
    buildChatProviderStatus(
      backend = "claude",
      ollamaModel = "gpt-oss",
      ollamaThinkLow = FALSE
    ),
    "Current provider: Claude"
  )
})

test_that("chat provider status helper reports Ollama mode", {
  expect_equal(
    buildChatProviderStatus(
      backend = "ollama",
      ollamaModel = "gpt-oss",
      ollamaThinkLow = FALSE
    ),
    "Current provider: Ollama (model: gpt-oss, normal thinking)"
  )

  expect_equal(
    buildChatProviderStatus(
      backend = "ollama",
      ollamaModel = "llama3.2",
      ollamaThinkLow = TRUE
    ),
    "Current provider: Ollama (model: llama3.2, low thinking)"
  )
})

test_that("chat provider error helpers centralise notification text", {
  expect_equal(
    buildUnknownChatProviderMessage(),
    "Unknown provider selected."
  )

  expect_equal(
    buildClaudeProviderIncorrectPasswordMessage(),
    "Incorrect password. Claude was not enabled."
  )
})

test_that("chat provider confirmation helper reports selected backend", {
  expect_equal(
    buildChatProviderSetMessage(
      backend = "claude",
      ollamaModel = "gpt-oss",
      ollamaThinkLow = FALSE
    ),
    "Chat provider set to Claude."
  )

  expect_equal(
    buildChatProviderSetMessage(
      backend = "ollama",
      ollamaModel = "gpt-oss",
      ollamaThinkLow = FALSE
    ),
    "Chat provider set to Ollama using model 'gpt-oss' with normal thinking."
  )

  expect_equal(
    buildChatProviderSetMessage(
      backend = "ollama",
      ollamaModel = "llama3.2",
      ollamaThinkLow = TRUE
    ),
    "Chat provider set to Ollama using model 'llama3.2' with low thinking."
  )
})


test_that("dummy chat provider helpers expose stored messages", {
  provider = structure(
    list(
      errorMessage = "ANTHROPIC_API_KEY is not set.",
      backend = "claude"
    ),
    class = "wmfm_dummy_chat_provider"
  )

  expect_true(isWmfmDummyChatProvider(provider))
  expect_equal(
    getWmfmDummyChatProviderMessage(provider),
    "ANTHROPIC_API_KEY is not set."
  )
  expect_false(isWmfmDummyChatProvider(list()))
  expect_null(getWmfmDummyChatProviderMessage(list()))
})

test_that("Claude provider without an API key gives a clear dummy-provider message", {
  oldKey = Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
  Sys.unsetenv("ANTHROPIC_API_KEY")
  on.exit({
    if (is.na(oldKey)) {
      Sys.unsetenv("ANTHROPIC_API_KEY")
    } else {
      Sys.setenv(ANTHROPIC_API_KEY = oldKey)
    }
  }, add = TRUE)

  provider = getChatProvider(backend = "claude")

  expect_true(isWmfmDummyChatProvider(provider))
  expect_match(
    getWmfmDummyChatProviderMessage(provider),
    "ANTHROPIC_API_KEY is not set",
    fixed = TRUE
  )
})
