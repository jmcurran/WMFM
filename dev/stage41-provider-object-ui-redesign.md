# Stage 41.5: Provider object UI redesign

## Purpose

This stage records the agreed design direction for simplifying WMFM provider setup before further implementation begins in Stage 41.6.

The current Settings screen exposes configuration mechanics too directly. New users see provider diagnostics, base URLs, model controls, and deployment notes before they have a simple mental model of what they need to do. This is confusing for desktop users and potentially unsafe or misleading when defaults expose institution-specific infrastructure.

The revised design treats a provider as a configured object. Users choose an active provider, and manage a list of configured providers. URLs, models, credentials, and diagnostics are secondary details that belong in add/edit dialogs or advanced panels, not on the main Settings screen.

## Core design principle

Users manage providers, not configuration variables.

A provider is a named endpoint that WMFM can use to generate explanations. A provider object encapsulates the provider type, endpoint, model choices, credential status, and deployment-management state.

The main Settings screen should answer three questions:

- Which provider is active?
- Which providers are available?
- Are those providers ready to use?

It should not require users to understand `.Renviron`, `config.json`, base URLs, environment variables, API-key storage, or deployment policy before they can get started.

## Proposed Settings layout

The main Provider settings section should have this structure:

```text
Provider settings  (i)

Active provider
[ <provider name> v ]

Providers
+--------------------------------------------------+
| Name              Type       Status             |
|--------------------------------------------------|
| Local Ollama      Ollama     Ready              |
| Claude            Claude     API key needed     |
+--------------------------------------------------+

[ + ] [ - ]
```

The active provider dropdown selects the provider WMFM will use for explanations.

The Providers table displays configured provider objects. It should show user-facing status, not secrets or low-level configuration.

The plus button opens an Add provider dialog. The minus button removes the selected provider after confirmation. Double-clicking or otherwise activating a provider row opens an Edit provider dialog.

The information icon should provide short user-facing guidance. Detailed diagnostics should remain behind a collapsed Advanced diagnostics section.

## Provider table columns

The preferred columns are:

- Name
- Type
- Status

Optional future columns may include:

- Model
- Last used
- Managed by administrator

The table should not show raw API keys, stored encrypted values, hashes, secret references, or institution-specific private URLs unless the user is explicitly editing that provider.

The earlier idea of a Hash column should be represented to users as Status. A one-way hash can confirm that a secret has been entered, but it cannot be used as the operational credential. If WMFM stores credentials locally, it needs either a recoverable encrypted credential or a reference to a credential stored elsewhere. The main UI should show only whether a credential is present and usable.

## Provider object model

A provider object should contain, conceptually:

```text
providerId
providerName
providerType
endpointUrl
model
credentialReference
isManaged
```

Possible provider types include:

- Ollama
- Claude

Future provider types may include OpenAI or other LLM backends.

`credentialReference` should not be displayed directly. It should refer to where the credential is stored or how it is resolved.

`isManaged` distinguishes administrator-provided providers from user-created providers.

## Credential design

Credential entry should happen in a modal dialog, not on the main Settings screen.

Desktop behaviour:

- Users may add or edit local provider credentials.
- API keys are entered through a provider setup dialog.
- API keys are never displayed after entry.
- The main provider table shows only a status such as `Ready`, `API key needed`, or `Credential stored`.

Server deployment behaviour:

- Ordinary users must not enter API keys.
- Ordinary users must not add arbitrary providers.
- Ordinary users must not edit administrator-managed provider credentials or endpoints.
- Administrators configure available providers and credentials during deployment.
- Users may select among administrator-approved providers and models when the deployment allows this.

## Fresh install behaviour

A fresh desktop install should not expose any institution-specific provider URL.

In particular, WMFM must not pre-fill Auckland-specific Ollama URLs such as internal department hosts. Those URLs are unusable for most users, may be behind a firewall, and could create inappropriate load if users try them.

For a fresh install, provider setup should be explicit:

- Ollama can be offered as a local provider type, but its endpoint should default to a generic local endpoint only when that is appropriate, such as `http://localhost:11434`.
- Claude can be offered as a cloud provider type, but it should show `API key needed` until configured.
- If no provider is ready, the app should guide the user to Add provider or Provider setup help.

## Add provider dialog

The Add provider dialog should ask for only the fields relevant to the selected provider type.

For Ollama:

- Provider name
- Endpoint URL
- Model
- Optional low-thinking/default behaviour if still needed
- Test connection or refresh models action

For Claude:

- Provider name
- Model if user-selectable
- API key setup action

The dialog should contain short guidance explaining what the provider is and what the user needs before saving.

## Edit provider dialog

The Edit provider dialog should allow a desktop user to update a provider they created.

It should allow:

- Rename provider
- Change endpoint URL where applicable
- Change model where applicable
- Replace credential where applicable
- Test provider readiness where feasible

It should not display existing API-key values.

For administrator-managed providers, the dialog should either be read-only or unavailable to ordinary users.

## Delete provider behaviour

Deleting a provider should require confirmation.

A provider should not be deleted if it is the only configured provider unless WMFM provides a clear recovery path. If the active provider is deleted, WMFM should select another available provider or mark the provider state as not configured.

Administrator-managed providers should not be deletable by ordinary users.

## Active provider behaviour

The active provider dropdown should contain only configured and allowed providers.

If no provider is usable, the dropdown should display a clear placeholder such as `No provider configured`, and the app should guide the user to setup.

Changing the active provider should update the provider used for future explanation requests.

## Advanced diagnostics

Advanced diagnostics should be collapsed by default.

They may include:

- Configuration file path
- Deployment mode
- Provider resolution source
- Credential source summary
- Environment-variable names
- Raw provider type and model identifiers

These details are for maintainers and support, not for ordinary first-run use.

## Migration from existing configuration

Stage 41.6 should preserve existing user settings.

Existing configuration should migrate to provider objects where possible:

- Existing Ollama URL/model settings become a Local Ollama provider object.
- Existing Claude environment-variable setup becomes a Claude provider object with credential status resolved from `ANTHROPIC_API_KEY`.
- Existing locally stored credentials, if present, become credential references.
- The existing active provider should become the active provider object.

Migration should be deterministic and tested offline.

## Testing goals for implementation

Stage 41.6 and follow-up implementation stages should test:

- Fresh install has no institution-specific Ollama URL.
- Fresh install shows a user-facing provider setup state.
- Active provider dropdown reflects configured providers.
- Provider table shows Name, Type, and Status.
- Add provider modal is available for desktop users.
- Edit provider modal can update user-created providers.
- Delete provider action confirms before removal.
- API keys are never displayed after entry.
- Deployed apps do not permit ordinary users to add, edit, delete, or credential providers.
- Existing config migrates without losing provider choices.
- Claude with `ANTHROPIC_API_KEY` remains supported.
- Ollama without credentials remains supported.

## Stage boundary

Stage 41.5 is design only.

The first code stage for this redesign should be Stage 41.6, not Stage 41.5.1. Patch-style stage numbers such as 41.5.1 should be reserved for repairs to completed 41.5 work, not for the first implementation of this design.

## Stage 41.6 first implementation slice

Stage 41.6 starts the provider-object redesign without trying to complete the whole registry architecture in one pass.

Implemented direction:

- The main Settings page now presents an active provider selector and a simple provider registry table with Name, Type, and Status columns.
- Add, remove, and setup actions are grouped with the provider registry rather than exposing technical config fields first.
- Advanced diagnostics and Ollama-specific connection fields are hidden behind expandable sections.
- The package default no longer exposes the University of Auckland Ollama URL to fresh users.
- Fresh local installs should show Ollama as needing setup rather than implying that a shared departmental endpoint is available.

Deliberately deferred:

- Full row-selection and double-click editing behavior.
- Provider IDs as the active selection value when multiple providers share the same provider type.
- Encrypted credential storage or keychain integration.
- Administrator-managed provider registry import/export for deployed servers.

These deferred items should be handled in later implementation stages after the new provider-object screen has been manually tested.

## Stage 41.7 active provider profile slice

Stage 41.7 continues the provider-object redesign by making the active provider selector profile-based rather than provider-type-based.

Implemented direction:

- The Active provider selector now uses configured provider profile identifiers as values.
- The active provider profile is persisted as `activeProviderProfileId` in the WMFM user configuration.
- Resolving the active provider now first looks for the selected provider profile and only falls back to legacy provider-type selection when needed.
- Ollama endpoint and model defaults can now come from the active provider profile.
- Adding or removing providers refreshes the active-provider selector choices.

Deliberately deferred:

- Row-selection and double-click edit behavior for the provider registry table.
- Full edit-provider modal support.
- Per-profile credential references beyond the current provider-type credential resolution.
- Administrator-managed provider registry import/export for deployed servers.
