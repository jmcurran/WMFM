# Stage 17.14 server inventory

## Structural extraction

Moved app-server state helper closures from `R/app-server.R` into `R/app-server-state-helpers.R`.

The top-level server now creates these closures through `createAppServerStateHelpers()` and keeps only the local bindings needed by later observers.

## Effect

This removes the bucket-state, model-page reset, and loaded-example input wiring helper definitions from the main server file while preserving their closure access to `input`, `session`, `rv`, and `modelFit`.
