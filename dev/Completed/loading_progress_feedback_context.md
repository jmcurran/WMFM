# WMFM Context: Loading progress feedback for list-box population

## Working title

Loading progress feedback for slow app initialisation

## Context

I am working on an R package/app called WMFM.

I have noticed that, while the app is loading, it is clearly doing several things before the relevant list box is populated. At the moment the user can experience a delay with little or no visible feedback, which makes the app feel unresponsive even when it is actually working.

I want to improve the student-facing experience by adding a progress indicator during this loading phase so that the user can see that the app is actively doing work.

Before starting any implementation in this workflow, please ask me to load the latest code base.

## Skills and workflow requirements

For this workstream, please use all of my existing skills where relevant:

- r-development-style
- wmfm-stage-script-generator
- wmfm-merge-tag-helper

Expectations:
- follow James's R development style rules
- use the WMFM staged development workflow
- provide downloadable change-set zip files for each implementation stage
- provide a stage script for each stage
- use plain-text ASCII git commit messages
- accumulate commit notes across bug-fix and test-fix iterations within a stage rather than replacing earlier notes
- preserve package-relative paths in change-set zips
- validate install zips before unzipping
- remove known stray top-level artifacts from earlier malformed installs when using `--install-files`

## Problem to solve

The current app appears to perform multiple startup or loading tasks before the relevant list box is ready for use. During that period:

- the user may see a delay
- the list box is not yet populated
- the interface gives too little feedback about what is happening

This creates a risk that users think the app has frozen or is not working.

## Goal

Add visible progress feedback during the loading sequence that leads to list-box population, so the user can tell that the app is still working and roughly where it is in the process.

The progress feedback should:

- appear while the relevant loading work is happening
- be understandable to non-technical users
- make the app feel responsive
- disappear or resolve cleanly once loading is complete

## High-level design intent

This work is about user experience, not just raw speed.

Even if the underlying startup work remains unchanged, the app should communicate:

- that it is busy
- that progress is being made
- when loading is complete

The first objective is not necessarily to optimise the loading pipeline itself. The first objective is to surface progress clearly and safely.

## Likely areas to inspect

Please begin by identifying:

- where the list box is populated
- what upstream computations or data preparation steps happen before that population
- whether those steps occur at app startup, on session initialisation, or reactively
- whether the current architecture supports incremental progress updates
- whether a Shiny progress bar, notification, modal, or lightweight status text would be the best fit

## Suggested development approach

### Stage 1: Trace the loading path

Inspect the current code path and identify:

- which list box is affected
- which server-side functions or reactive chains feed it
- what steps are making the process slow enough to be visible
- whether the slowness is due to computation, file loading, model preparation, UI rendering, or repeated reactive work

Deliverable:
- targeted findings only
- no redesign yet unless clearly needed

### Stage 2: Add basic progress feedback

Implement an initial progress indicator around the loading path.

Possible options include:

- `withProgress()` / `incProgress()`
- a visible loading message near the list box
- a modal or notification that the app is preparing content
- temporary placeholder UI until the list is ready

Requirements:
- keep the implementation simple and robust
- prefer deterministic UI feedback over clever but fragile behaviour
- make the wording student-friendly

### Stage 3: Improve progress granularity if supported

If the loading path has identifiable sub-steps, consider exposing them, for example:

- loading data
- preparing model information
- building available choices
- finishing interface setup

This should only be done if the sub-steps map cleanly onto the actual code path.

Do not invent fake precision if the workflow does not naturally support it.

### Stage 4: Harden behaviour and edge cases

Check cases such as:

- very fast loads where the progress UI should not flicker awkwardly
- failed loads
- partial loads
- repeated triggering of the same loading path
- session restart or app refresh
- empty-state behaviour if the list has no valid choices

### Stage 5: Tests and manual checks

Add tests where practical, and document manual checks for UI behaviour.

Likely focus:
- helper-level tests for any new progress-related utilities
- server tests if the package already has a pattern for these
- manual confirmation that the list box becomes available cleanly and that progress messaging is understandable

## Non-goals

At least initially, this work should not automatically expand into:

- a full performance optimisation pass across the app
- a redesign of unrelated UI panels
- broad refactoring of app startup logic unless required
- speculative asynchronous infrastructure unless clearly justified

If real performance bottlenecks are discovered, those can become a separate later workstream.

## Design preferences

Prefer:

- small, targeted changes
- student-facing wording
- robust Shiny-native patterns
- behaviour that is easy to test and reason about

Avoid:

- misleading fake progress
- over-engineered reactive complexity
- major startup refactors unless evidence shows they are necessary
- developer-facing wording in the visible UI

## Deliverable expectations for the implementation chat

When working on this context in a new chat:

- first ask me to load the latest code base
- then inspect the list-box loading path
- propose the narrowest useful first implementation
- deliver each stage as:
  - a downloadable change-set zip
  - a WMFM stage script
  - a plain-text git commit message
  - short change notes

## Suggested chat title

Loading progress feedback for list-box population
