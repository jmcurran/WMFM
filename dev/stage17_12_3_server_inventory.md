# Stage 17.12.3 server inventory

This repair keeps the Stage 17.12 structural extraction and updates the
source-inspection test for the new file boundaries.

- `verifyDeveloperModePassword()` is defined in `R/utils-developerModeAuth.R`.
- Developer-mode observer wiring is no longer expected to live only in
  `R/app-server.R` after the startup observer extraction.
- The test now combines the server, startup observer, developer-mode helper,
  and developer-mode authentication files before checking that developer-mode
  access remains password protected.
