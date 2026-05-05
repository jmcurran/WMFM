# Stage 17.12 server inventory

## Structural extraction

Stage 17.12 moves the startup data-choice observer block out of `R/app-server.R`
and into `R/app-server-startup.R`.

The new `registerStartupDataChoiceObservers()` function owns:

- initial package and example reactive values
- package scan status rendering
- package dataset status rendering
- example loading status rendering
- startup `session$onFlushed()` package/example scans
- package dataset choice updates

`appServer()` now calls the registration helper and keeps only the reactive
values needed by later server sections.

## Expected effect

This is the first Stage 17 structural extraction. It should reduce
`R/app-server.R` by roughly 200 lines and make the opening of `appServer()`
easier to read without changing user-facing behaviour.
