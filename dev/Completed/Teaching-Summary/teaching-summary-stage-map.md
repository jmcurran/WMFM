# WMFM teaching-summary rebuild stage map

This note renumbers the uploaded rebuild plan so the new stream starts at 4.6.5.

## Renumbered stages

- 4.6.5: inspect old implementation and recover feature intent
- 4.6.6: define the current data contract
- 4.6.7: implement the core teaching-summary builder
- 4.6.8: implement rendering helpers
- 4.6.9: integrate into the app
- 4.6.10: testing and polish

## Important finding from the attached 4.6.4 snapshot

The attached `stage4.6.4_completed.zip` snapshot already appears to contain a substantial teaching-summary implementation in the current codebase, including:

- `R/model-explanationTeachingSummary.R`
- `R/app-explanationTutor.R`
- `R/app-explanationAudit-uiHelpers.R`
- teaching-summary wiring in `R/api-runModel.R`
- teaching-summary rendering and tutor integration in `R/app-server.R`
- focused tests in `tests/testthat/test-buildExplanationTeachingSummary.R`

## Recommended interpretation

Because the current snapshot already contains these pieces, stage 4.6.5 should be treated as a verification and architecture-review pass before any further rebuild work.

Recommended 4.6.5 outputs:

- confirm whether the snapshot is the intended current baseline
- inspect whether the current teaching-summary code is the rebuild target or only a partial carry-over
- write a concise design note that separates:
  - deterministic teaching-summary generation
  - optional tutor-style expansion
  - internal audit and claim-evidence responsibilities
- identify any remaining gaps between the uploaded context note and the actual stage 4.6.4 code snapshot

## Suggested immediate next move

Use 4.6.5 to produce a short gap-analysis note comparing:

- the rebuild intent in the context document
- the currently implemented teaching-summary surface in the 4.6.4 snapshot
- any missing or still-overcoupled pieces that should be cleaned up in 4.6.6 and later
