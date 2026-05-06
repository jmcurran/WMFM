# Stage 17.13.3 server inventory

Stage 17.13.3 is a test-only repair for the chat-provider observer extraction.

- Updates `test-app-server-chat-provider-observers.R` to use package-root source lookup.
- Avoids brittle `test_path("..", "..", ...)` paths under `R CMD check`.
- Keeps the Stage 17.13 structural extraction unchanged.
