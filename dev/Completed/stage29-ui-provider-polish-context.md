# Stage 29 - UI polish, provider persistence, and developer-panel cleanup

## Branch

Suggested branch name:

```text
ui-provider-polish
```

## Background

Stage 28 focused on log-log model support and the Diamonds II, III, and IV examples. During manual testing, several UI and workflow issues were identified that are not specific to log-log modelling. These should be handled in a separate stage/workstream rather than extending the statistical modelling work.

The guiding principle for this stage is that user-facing controls should behave transparently, retain useful session preferences where safe, and distinguish clearly between real user input and placeholders or developer diagnostics.

## Main goals

1. Improve small but confusing UI details found during Stage 28 manual testing.
2. Persist useful local/provider preferences between sessions.
3. Harden Ollama model discovery so it works gracefully across different local and institutional setups.
4. Clean up developer-panel naming and layout so developer diagnostics look and behave consistently.
5. Remove stage-number naming from lasting test files or functions.

## Deferred UI issues from Stage 28

### 1. Follow-up placeholder styling

The optional follow-up question box displayed placeholder text that looked too much like a real question. This caused confusion during Diamonds II testing because the example correctly had no follow-up question, but the placeholder looked like one.

Requirements:

- Make placeholder text visibly lighter than entered text.
- Keep the placeholder short and neutral.
- Suggested placeholder:

```text
Optional: ask a follow-up question about this fitted model.
```

- Placeholder styling should make it visually obvious that the text is not active content.
- Do not remove the placeholder entirely unless the UI still remains discoverable.

### 2. Explanation prompt diagnostics accordion styling

The developer prompt diagnostics panel behaves like an accordion but does not visually look like one. During Stage 28 testing, it appeared as a plain heading even though it could expand/collapse.

Requirements:

- Make `Explanation prompt diagnostics` visually consistent with the other accordion sections.
- Align styling with sections such as:
  - `How each sentence was supported`
  - `How to read this explanation`
  - `Optional AI tutor`
- The visual affordance should match the behaviour.
- Avoid introducing a separate one-off style for developer diagnostics.

### 3. Developer panel organisation

Review developer-only panels for consistency and discoverability.

Potential areas:

- Explanation prompt diagnostics
- Support map / claim evidence views
- Prompt excerpts
- Follow-up diagnostics
- AI tutor/developer sections

Requirements:

- Keep developer diagnostics available in developer mode.
- Avoid cluttering the student-facing UI.
- Prefer consistent accordion/card styling.
- Do not expose internal diagnostics in normal student mode.

## Provider and developer-mode persistence

Manual testing is slowed down by repeatedly selecting the same provider/model and re-enabling developer mode.

### Desired behaviour

Persist these preferences between app sessions where safe:

- last selected provider
- last selected model, if applicable
- developer mode enabled/disabled state

### Design constraints

- Store preferences in the existing app/user config mechanism if one exists.
- Do not store API keys or secrets in unsafe locations.
- If the previous provider/model is no longer available, fall back gracefully.
- Developer mode persistence should be explicit and local; avoid surprising users in shared environments.
- Add tests for preference read/write behaviour using temporary config files or mocks.

## Ollama model discovery hardening

Ollama availability and model discovery may behave differently across machines and institutions. The current assumptions may be too specific to one local setup.

### Requirements

- Make Ollama model discovery optional or gracefully degradable.
- Avoid blocking the UI when model discovery fails.
- Show a clear, non-alarming message when Ollama is unavailable or cannot list models.
- Allow a manually configured Ollama model where discovery is not possible.
- Avoid assuming one institutional network/proxy behaviour applies to all users.
- Tests must mock Ollama responses and failures; do not require a real Ollama service.

### Possible behaviours

- If discovery succeeds: populate available models normally.
- If discovery fails: keep the provider selectable only if a configured model is available, or show a clear disabled state.
- If a previous Ollama model was saved: allow it to remain as a manual/default choice, with a warning if it cannot be verified.

## Naming cleanup: no stage numbers in lasting source/test names

During Stage 28, we noted a test file named something like:

```text
test-provider-settings-stage22-6.R
```

This should be cleaned up as part of the provider work.

### Rule

Stage numbers are workflow metadata. They should not appear in lasting code, test, or function names.

### Allowed places for stage numbers

- generated stage scripts
- generated stage archives
- generated change zips
- dev notes
- commit messages
- temporary stage-specific context files

### Not allowed in lasting project code

- R function names
- exported object names
- long-lived test filenames
- app IDs
- user-facing labels

### Suggested rename

```text
tests/testthat/test-provider-settings-stage22-6.R
```

to:

```text
tests/testthat/test-provider-settings.R
```

or another stable feature-based name.

## Non-goals for this stage

Do not include the derived-variable provenance registry in this UI/provider stage. That should remain a separate modelling infrastructure workstream.

Do not redesign the Stage 28 log-log interpretation system unless a direct UI/provider issue requires a small compatibility fix.

Do not make network-dependent tests.

Do not require a real OpenAI, Anthropic, Ollama, or other provider connection in tests.

## Testing expectations

Add or update deterministic offline tests for:

- placeholder styling or generated UI HTML/classes where testable
- accordion consistency for developer prompt diagnostics
- provider preference persistence
- developer mode persistence
- missing/unavailable provider fallback
- Ollama discovery success and failure via mocks
- removal/renaming of stage-numbered provider test files

Tests should not call real external services.

## Development conventions

Continue to use:

- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation for functions
- modular helper functions
- offline deterministic tests
- thin orchestration layers
- no real LLM/provider calls in tests

## Suggested stage numbering

If Stage 28 is closed after 28.8.13, this can be:

```text
Stage 29.1
```

for the first UI/provider polish step.

If the work is done as a new standalone stream, use stage numbers consistently within that stream and keep durable file/function names feature-based rather than stage-based.
