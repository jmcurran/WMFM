# Stage 17.7 server inventory

## Focus

Stage 17.7 continues the app-server refactor by extracting data-loading notification text from `R/app-server.R`.

## Extracted area

The extracted helpers cover user-facing messages for:

- failed delimited upload reads
- RDA/RData files without data frames
- unsupported upload extensions
- missing custom TXT separators
- package dataset load failures
- selected package objects that are not data frames
- missing example selection

## Rationale

These messages are pure, stable, and low-risk. Moving them out of the monolithic server keeps behaviour unchanged while reducing the amount of inline text and decision detail inside `appServer()`.

## Next candidates

Likely next extraction targets are data-context notifications or model-fitting guard messages.
