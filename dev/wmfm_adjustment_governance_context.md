# WMFM Context — Adjustment-Aware Explanation Governance and Scoring Alignment

## Repository and branch context

Repository:
- GitHub: jmcurran/WMFM

Primary branch:
- adjustment-variables

Current development phase:
- Post-implementation stabilization and policy alignment for adjustment-variable workflows.

The core adjustment-variable implementation is now broadly functional and manually testable. The project has transitioned from:
- feature implementation
to:
- explanation-governance refinement
- scoring alignment
- deterministic post-processing cleanup
- prompt/test architecture stabilization

---

# Current state of the adjustment workflow

The adjustment-variable system now supports:
- selection of adjustment variables in the UI
- adjusted model fitting
- prompt scaffolding for adjustment-aware explanations
- suppression of adjustment-variable narrative leakage
- hiding adjustment-variable outputs in summaries/CIs
- adjustment-aware explanation generation
- interaction-aware suppression rules

The workflow has been tested manually using the `s20x::arousal.df` example.

Example model:

```r
arousal ~ gender + picture + gender:picture
```

with:
- `picture` selected as an adjustment variable.

Research question:

> Is there a difference in arousal levels for females and males?

---

# Important conceptual transition

The project is now exposing a deeper architectural issue:

Historically, WMFM explanations and scoring assumed:

> more interpretation is always better.

The new adjustment-aware framework instead assumes:

> only scientifically justified interpretation should appear.

This changes:
- explanation style,
- interaction narration,
- scoring philosophy,
- prompt policy,
- and test design.

The system is now entering an:
- explanation governance
- interpretation policy
- scoring alignment
phase.

This is intentional and should NOT be reverted accidentally.

---

# Current explanation behavior

## Interaction model behavior (GOOD)

Example explanation:

> Researchers asked whether arousal levels differ between females and males. In the analysis, arousal was measured while participants viewed a variety of pictures, and the statistical model was built to account for any differences that might arise from the particular pictures shown. After adjusting for these picture effects, the results do not indicate a clear difference in arousal between genders, suggesting that gender appears not to influence arousal in this setting.

This is considered conceptually much healthier because:
- picture remains background context,
- picture levels are not narrated,
- interaction cells are not interpreted directly,
- the explanation remains focused on the research question,
- the model is not explained coefficient-by-coefficient.

This restrained style is now intentional.

---

## Additive model behavior (PARTIALLY PROBLEMATIC)

Example explanation:

> The study asked whether arousal levels differ between females and males. After accounting for the picture shown, the adjusted comparison indicates that males’ arousal levels are about 2.1 units lower than those of females. the interval for the difference ranges from 4 units lower to 0.15 units lower, which suggests a consistent reduction in arousal for males compared with females.

Observed issues:
- explanation feels deterministic/template-driven,
- repeated capitalization bug:
  > "the interval..."
- explanation is technically correct but pedagogically flat,
- reads more like a regression summary than a teaching explanation,
- suggests deterministic post-processing is now heavily shaping the final text.

This is NOT primarily an LLM issue anymore.
It is likely a deterministic explanation-composition issue.

---

# Current adjustment interpretation policy

The following policies are now considered correct and should be preserved unless intentionally redesigned.

## Allowed
- mentioning adjustment variables as adjusted-for context,
- high-level statements that the model accounts for adjustment variables,
- high-level acknowledgment that interaction structure exists,
- restrained conclusions,
- uncertainty-aware language.

## Forbidden
- narrating adjustment-variable coefficients,
- narrating adjustment-variable contrasts,
- narrating adjustment-variable confidence intervals,
- narrating adjustment-variable fitted means,
- narrating picture-level cell comparisons,
- using adjustment variables as narrative conditioning axes,
- explaining the regression table instead of the research question.

---

# Important interaction policy

Current preferred interpretation rule:

> Adjustment variables may participate in high-level interaction summaries, but may not become narrative conditioning axes with level-specific estimates.

This means:
- high-level interaction acknowledgment may be acceptable,
- but:
  - "for infant pictures..."
  - "for nude-female images..."
  - cell-specific conditional narration
  should remain suppressed.

---

# Current scoring problem

The scoring/grading system appears misaligned with the new explanation policy.

Observed behavior:
- restrained adjustment-aware explanations receive mediocre scores,
- explanations are likely penalized for:
  - lack of detailed interpretation,
  - absence of picture-specific narration,
  - reduced numeric detail,
  - missing interaction cell discussion.

This likely reflects older grading assumptions:

> detailed coefficient interpretation is inherently better.

The grading system now needs adjustment-aware policy alignment.

---

# Likely future stages

## Stage 20.19 — Adjustment-aware scoring policy

Goals:
- teach the grader that restrained explanations may be correct,
- avoid penalizing missing adjustment-level narration,
- preserve scoring behavior for non-adjustment workflows,
- reward:
  - correct adjusted-for framing,
  - restraint,
  - research-question focus,
  - avoidance of forbidden adjustment narratives.

Potential rubric changes:
- do not require picture-level interpretation,
- do not require interaction cell narration,
- do not reward coefficient-table recitation,
- distinguish:
  - "missing important interpretation"
  from
  - "correctly suppressed interpretation."

---

## Stage 20.20 — Explanation governance cleanup

Potential goals:
- unify explanation policy logic,
- reduce contradictory prompt instructions,
- centralize interpretation governance,
- formalize:
  - allowed narratives,
  - forbidden narratives,
  - interaction handling rules.

---

## Stage 20.21 — Deterministic post-processing polish

Potential goals:
- improve explanation fluency,
- fix stitching/capitalization artifacts,
- reduce repetitive deterministic phrasing,
- improve paragraph composition,
- make restrained explanations feel more natural.

Important:
Do not reintroduce unsafe adjustment-level narration while improving fluency.

---

## Stage 20.22 — Prompt audit and invariant-based testing cleanup

Current issue:
Many prompt tests assert exact wording.

This is becoming brittle and slowing development unnecessarily.

Preferred direction:
Test policy invariants instead of exact prose.

Good tests:
- prompt mentions adjustment variables,
- prompt includes adjusted-for framing,
- prompt forbids adjustment-level narration,
- payload excludes adjustment-level rows,
- no-adjustment behavior unchanged.

Bad/brittle tests:
- exact full-sentence matching,
- exact prose ordering,
- wording-level snapshots of entire prompt sections.

Exact wording tests should remain ONLY for:
- truly contractual safety/policy phrases.

---

# Important implementation philosophy

The goal is NOT:
- maximal verbosity,
- maximal coefficient narration,
- or maximal statistical detail.

The goal is:
- scientifically responsible explanation,
- pedagogically appropriate interpretation,
- alignment with the research question,
- and avoidance of misleading adjustment-variable narratives.

Sparse explanations may sometimes be the correct outcome.

---

# R development conventions

Continue following project conventions:
- camelCase identifiers,
- braces on all control structures,
- roxygen2 documentation,
- prefer @importFrom over ::,
- modular architecture,
- thin orchestration layers,
- avoid business logic in app-server.R.

Generated docs must remain synchronized:

```r
devtools::document()
```

Validation workflow:

```r
R -q -e "devtools::document()"
R -q -e "devtools::test()"
R -q -e "devtools::check()"
R -q -e "shiny::runApp()"
```

---

# Important CI lesson

Codex environments often cannot run R.

Do not trust claims that:
- devtools::document()
- devtools::test()
- devtools::check()

passed unless R actually ran.

GitHub Actions remains authoritative.

For difficult stabilization/test-policy work:
- prefer local validation loops over repeated Codex guess-and-check cycles.
