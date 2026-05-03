# Stage 17.5 server inventory

## Focus

Stage 17.5 continues the low-risk helper extraction pattern by moving chat-provider status and notification wording out of `R/app-server.R`.

## Extracted responsibilities

- Current chat provider status text
- Unknown chat provider error text
- Incorrect Claude-provider password text
- Chat provider selection confirmation text

## Files added

- `R/app-server-chat-provider-helpers.R`
- `tests/testthat/test-app-server-chat-provider-helpers.R`

## Server changes

`R/app-server.R` now delegates chat-provider wording to pure helper functions. The server still owns the Shiny reactivity, password checks, input updates, and notifications.

## Behaviour note

The user-facing text is intended to remain unchanged. This stage only moves wording construction into testable pure functions.
