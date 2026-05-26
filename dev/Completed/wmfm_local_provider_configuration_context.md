# WMFM Future Workstream Context - Local Provider Configuration

## Background

Stage 19 focused on repeated scoring robustness and semantic calibration.

Key outcomes included:

- elimination of major SG-3 and SG-4 semantic instability cliffs
- preservation of deterministic aggregation architecture
- configurable repeated-scoring audit tooling
- regression protections for semantic calibration behavior
- improved provider-key error messaging

Stage 19 is now considered merge-ready.

---

# Proposed next major workstream

The next major architectural workstream should likely focus on:

```text
local provider configuration and persistence
```

rather than continued semantic calibration.

The goal is to make the WMFM application provider-agnostic and user-configurable while preserving offline-friendly behavior and reproducibility.

---

# Core architectural goals

## 1. User-configurable providers

Users should be able to configure their own:

- LLM provider
- model name
- API endpoint
- API keys
- generation defaults

Examples:

- OpenAI
- Anthropic / Claude
- Ollama
- local OpenAI-compatible servers
- future providers

The application should not assume a single provider architecture.

---

## 2. Local-first configuration storage

Configuration should be stored locally whenever possible.

Examples of locally stored information:

- selected provider
- default model
- provider-specific settings
- API keys
- temperature/token defaults
- local endpoint URLs

Important principle:

```text
Current hard-coded defaults should also become locally configurable.
```

This includes existing provider defaults already embedded in the app.

---

## 3. Separate local vs server deployment behavior

A likely architectural complication ("fish hook") is that:

```text
local desktop/dev installs
```

and:

```text
shared Shiny server deployments
```

probably require different configuration strategies.

### Local desktop/dev installs

Local installs can likely use:

- local config files
- local encrypted storage
- environment variables
- user-owned API keys

### Shared Shiny deployments

Server deployments may require:

- per-user secure credential storage
- environment-managed secrets
- provider restrictions
- admin-controlled defaults
- session-isolated configuration

The app architecture should avoid assuming that local-storage approaches automatically work on shared infrastructure.

---

# Important design principles

## Preserve provider abstraction

Provider-specific logic should remain isolated behind a provider abstraction layer.

Avoid scattering provider-specific code throughout:

- app server code
- grading logic
- semantic scoring
- UI rendering

---

## Preserve offline-friendly testing

Tests must continue to:

- run offline
- avoid real LLM calls
- use mocks/fake providers
- remain deterministic

Provider configurability should not weaken test reproducibility.

---

## Preserve deterministic scoring architecture

This workstream should remain separate from:

- semantic calibration
- rubric weighting
- grading architecture

The provider configuration layer should not alter deterministic scoring behavior.

---

# Suggested future stages

## Stage 20.x

Potential sub-stages:

### Stage 20.1

Provider configuration abstraction audit.

Goals:

- identify current provider assumptions
- identify hard-coded defaults
- isolate provider configuration responsibilities

---

### Stage 20.2

Local configuration persistence.

Goals:

- config-file strategy
- provider registry design
- default model persistence
- migration strategy for existing defaults

---

### Stage 20.3

Secure credential handling.

Goals:

- local secret handling
- environment-variable integration
- optional encrypted storage
- Shiny deployment compatibility

---

### Stage 20.4

Provider management UI.

Goals:

- provider selection UI
- editable defaults
- provider testing
- provider health/status display

---

### Stage 20.5

Shiny deployment strategy.

Goals:

- multi-user credential strategy
- session isolation
- admin policy controls
- deployment documentation

---

# Suggested next-chat opener

```text
We completed Stage 19 semantic calibration and repeated scoring audit work and merged it into master.

I now want to begin planning a new workstream focused on local provider configuration, provider abstraction, and local persistence for WMFM. Here is the architectural context document.
```
