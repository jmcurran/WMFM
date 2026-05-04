# Stage 17.13 server inventory

## Structural extraction

Moved the chat-provider observer block out of `R/app-server.R` into `R/app-server-chat-provider.R`.

## Extracted responsibility

`registerChatProviderObservers()` now owns:

- Ollama model choice refreshes
- chat provider status rendering
- provider switch validation
- provider-switch password handling
- provider switch notifications

## Effect on app-server.R

The main server now delegates chat-provider observer setup through a single call, reducing the size of the orchestration function while preserving existing behaviour.
