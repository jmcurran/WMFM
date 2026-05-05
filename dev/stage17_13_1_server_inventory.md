# Stage 17.13.1 server inventory

## Purpose

Fix the source-boundary test that still assumed the Ollama low-thinking observer lived directly in `R/app-server.R`.

## Change

- Updated `tests/testthat/test-app-ollama-think-low-ui.R` to read both `R/app-server.R` and `R/app-server-chat-provider.R`.
- Kept the test asserting that the app server initialises `activeOllamaThinkLow`.
- Moved observer-specific assertions to the combined source text now that chat-provider observers are extracted.

## Behaviour

No app behaviour is changed. This is a test-only boundary update after the Stage 17.13 structural extraction.
