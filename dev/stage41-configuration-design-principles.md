# Stage 41.1 configuration design principles

## Purpose

Stage 41 is intended to make WMFM easier to configure for first-time users without weakening the security boundary for deployed Shiny applications.

Stage 41.1 is deliberately a design and documentation stage. It records the principles that should guide later implementation work before changing the configuration code, Settings UI, provider setup logic, tests, or README setup instructions.

## Current understanding

WMFM currently has two different kinds of configuration information:

- ordinary app preferences, such as selected providers or local provider settings
- credentials and secrets, such as API keys

The app already has a per-user WMFM configuration location. The configuration location should be treated as user-specific state rather than package source code. API keys are currently expected to come from the user's R environment, usually through `.Renviron` or the process environment.

The main usability concern is that a new desktop user may not know where to put provider settings or API keys. The main security concern is that a Shiny Server or other multi-user deployment must not encourage per-user or browser-entered secrets to be written into a shared server-side configuration file.

## User groups

### Single-user desktop users

For a local desktop user, the app should be easy to start and should explain what is missing when a provider cannot be used. The user should not need to understand all internal configuration files before using WMFM.

The design may permit a single-user convenience path for storing credentials in the WMFM user configuration, but only if the app clearly distinguishes this from server deployment guidance.

### Shiny Server and other deployed uses

For a deployed app, secrets should be supplied through server-controlled environment variables or deployment secrets. The app should not invite ordinary browser users to save API keys into a shared app configuration file.

A deployed app should have one clear administrator-facing setup route. End users of the deployed app should see provider availability and missing-provider guidance, not raw secret-management controls.

## Security principles

- Treat API keys, tokens, and provider credentials as secrets.
- Do not store secrets in package source files, examples, tests, README snippets, committed config files, or generated stage artefacts.
- Prefer environment variables for Shiny Server, Posit Connect, Docker, CI, and other deployed contexts.
- If local config-file credential storage is implemented later, make it explicitly single-user oriented.
- Give environment variables priority over any local config value so administrators can override user-level settings.
- Never display full credential values in the UI, logs, tests, NEWS, or README examples.
- Provide a way to remove any locally stored credential if local credential storage is eventually supported.
- Keep tests deterministic and avoid real provider calls or real secrets.

## Simplicity principles

- A new user should be able to open Settings and understand what is configured, what is missing, and what to do next.
- Provider status should use plain language such as `available`, `not configured`, or `requires API key`.
- Setup instructions should explain the recommended path before mentioning alternatives.
- The first-run experience should not require the user to find a hidden config path before doing anything useful.
- Error messages should name the missing provider and the expected environment variable where appropriate.
- The UI should avoid implying that every deployment mode supports the same credential-storage behaviour.

## Proposed configuration precedence

Later implementation should use one clear precedence order:

1. Explicit provider argument supplied by package code or tests.
2. Environment variable or process-level deployment secret.
3. WMFM user configuration for non-secret preferences.
4. Optional WMFM user configuration credential only when local single-user storage is allowed.
5. Built-in defaults.

Environment variables should win over local config values. This keeps deployed and scripted use predictable.

## Proposed configuration categories

### Safe to store in WMFM user config

- selected provider name
- local Ollama base URL
- preferred local model name
- non-secret UI preferences
- non-secret provider defaults

### Not safe to store by default

- API keys
- bearer tokens
- account-specific service credentials
- values that allow paid model usage

### Conditionally acceptable for single-user local storage

- API keys, only if later design explicitly opts into local single-user convenience storage and the UI explains the trade-off

## Deployment detection and policy

Later implementation may include a helper that decides whether local credential storage is allowed. That policy should be conservative.

Local credential storage should be disabled when the app appears to be running in a deployed or multi-user context, including Shiny Server, Posit Connect, CI, container deployment, or any explicit WMFM environment flag that disables config-file secrets.

The design should also allow an administrator to disable local credential storage explicitly even on systems that are difficult to classify automatically.

## UI design principles

- Separate provider selection from credential management.
- Show provider status without exposing secrets.
- Make environment-variable setup the recommended route for deployed use.
- Make local convenience storage, if implemented later, visibly local-only.
- Avoid adding secret controls to the UI until the policy and copy are agreed.
- Include reset/remove actions for any future stored credential path.
- Keep Settings wording educational but brief.

## Documentation principles

The README should eventually explain:

- how to run WMFM with Ollama without commercial API keys
- how to configure Claude or other hosted providers using environment variables
- how to configure Shiny Server or Posit Connect deployments safely
- what WMFM stores in its user config file
- what WMFM does not store by default
- how a local desktop user can check provider readiness

README examples must never include real-looking API keys.

## Testing principles

Later implementation tests should cover:

- configuration read/write behaviour without touching the user's real home directory
- precedence of environment variables over config-file values
- server/deployment disabling of local credential storage
- masked status text for configured credentials
- README setup guidance
- no real provider calls and no real secrets

## Open design decisions

Before implementation resumes, decide:

1. Should WMFM support storing API keys in the WMFM user config for local desktop use at all?
2. If yes, should that be opt-in through a setting, an environment flag, or both?
3. Should the Settings UI provide a credential-entry field, or only explain environment-variable setup?
4. Should a deployed app hide credential-entry controls entirely?
5. Which providers need first-class configuration support in the first implementation stage?
6. Should OpenAI support be documented now or deferred until it has a concrete provider implementation?

## Proposed later implementation stages

### Stage 41.2: configuration policy helpers

Add small, tested helpers for provider configuration policy, deployment detection, config-path handling, and credential precedence. Avoid UI changes in this stage.

### Stage 41.3: Settings UI copy and status

Improve Settings status messages and setup guidance without adding credential persistence unless Stage 41.1 decisions explicitly allow it.

### Stage 41.4: README setup guide

Update the README with desktop and deployed setup instructions after implementation behaviour is stable.

### Stage 41.5: optional local credential storage

Only implement this stage if the design decision is to support single-user config-file credential storage. Keep the implementation conservative, reversible, masked, and disabled for deployed contexts.


## Stage 41.2 implementation decision

Stage 41.2 begins with policy and user-experience guardrails rather than API-key persistence. The first implementation slice should:

- detect administrator-managed deployments conservatively;
- prevent ordinary deployed-app users from editing provider configuration;
- keep provider and credential guidance in a modal dialog rather than front-and-centre Settings text;
- allow local desktop users to continue editing non-secret provider settings;
- leave actual credential persistence for a later stage after the single-source configuration decision is fully settled.

This keeps the deployed-app security boundary clear while avoiding another premature credential-storage implementation.
