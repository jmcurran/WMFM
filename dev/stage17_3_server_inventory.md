# Stage 17.3 server inventory

## Scope

Stage 17.3 continues the app-server refactor by extracting package dataset
status and notification wording from `R/app-server.R` into pure helper
functions in `R/app-server-startup-helpers.R`.

## Refactor notes

- Kept the existing package dataset reactive flow inside `appServer()`.
- Centralised user-facing text for package dataset scanning, missing `s20x`,
  empty package datasets, and found dataset counts.
- Added focused unit tests for the new pure helpers.

## Behaviour

No user-facing behaviour is intended to change. The same messages are emitted
from the same server events; only the construction of those messages has moved
out of the monolithic server function.

## Next candidates

- Extract package dataset choice construction helpers.
- Extract example-loading status helpers.
- Continue moving low-risk, pure message construction out of `appServer()`
  before touching model-fitting or plotting reactivity.
