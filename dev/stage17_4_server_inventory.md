# Stage 17.4 server inventory

## Scope

Stage 17.4 continues the low-risk app-server wording extraction started in
Stages 17.1 to 17.3.

## Extracted responsibility

The transient startup example and package-scan messages now live in
`R/app-server-startup-helpers.R` as pure helper functions. This keeps the
startup observer focused on orchestration and leaves the wording testable
outside Shiny reactivity.

## Behaviour notes

- No reactive flow was changed.
- No UI IDs were changed.
- The startup notification and status text are intended to remain identical.
- The server still owns the sequencing of startup work.

## Suggested next step

Stage 17.5 should start extracting selection-default helper logic for the
example and package selectors, because those branches are still pure and
can be tested without a Shiny session.
