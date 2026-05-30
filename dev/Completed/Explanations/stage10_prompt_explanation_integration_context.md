# Stage 10 Context: Prompt and Explanation Integration

## Context

Stage 9 is complete on the `explanation-fix` branch.

Stage 9 deliberately focused on infrastructure rather than changing student-facing explanations. It added deterministic support for model profiling, formatting, anchor selection, model-aware rules, sentence schema metadata, validation flags, and developer-facing diagnostics.

The goal of Stage 10 is to use that infrastructure to improve the actual explanation-generation path.

## Completed Stage 9 infrastructure

Stage 9 added or refined these layers:

- explanation model profiles
- deterministic formatting helpers
- explanation anchor helpers
- model-aware explanation rule profiles
- Stage 9 sentence schema fields
- deterministic validation and quality flags
- developer-feedback and developer-mode UI surfacing

Important Stage 9 design decisions:

- Multi-role tagging is required.
- Legacy `claimTags` must remain backward-compatible.
- Expanded Stage 9 roles belong in `roles`, not by changing the legacy meaning of `claimTags`.
- Student-facing explanations should remain clean; Stage 9 metadata is primarily for developer support and future prompt control.
- Deterministic layers should compute, format, and validate whenever possible.
- The LLM should describe, not decide.

## Stage 10 goal

Stage 10 should connect the new deterministic infrastructure to the prompt and explanation-generation pipeline.

The main goal is to improve generated explanations by feeding the LLM cleaner, more structured, less ambiguous inputs.

The focus should be on:

- using formatted quantities rather than raw model quantities
- using model-rule profiles to select explanation skeletons
- reducing log-scale and coefficient-scale leakage
- improving interaction explanations
- controlling excessive treatment/group comparisons
- keeping uncertainty and estimates together
- preserving strong existing behaviour

## Stage 10 branch

Use this branch for Stage 10 work:

```text
prompt-explanation-integration
```

## Working assumptions

The completed Stage 9 codebase is available from:

```text
stage9_7_completed.zip
```

The completed Stage 9 branch is:

```text
explanation-fix
```

The Stage 10 branch should be created from updated `master` after merging `explanation-fix`.

## Suggested Stage 10 implementation sequence

### Stage 10.1 - Audit current prompt inputs

Inspect the current prompt-building path and identify where raw or partially processed values enter the explanation prompt.

Likely files or areas to inspect:

- prompt construction helpers
- explanation audit construction
- support-map construction
- model explanation UI/server path
- equation/explanation runtime path

Output:

- a short map of prompt inputs
- a list of raw quantities that should be replaced by formatted Stage 9 quantities
- no broad prompt rewrite yet

### Stage 10.2 - Feed formatted quantities into prompts

Use the Stage 9 formatting helpers so the LLM receives student-ready quantities.

Examples:

- formatted fitted values
- formatted confidence intervals
- formatted odds
- formatted multipliers
- formatted anchors

Rules:

- do not ask the LLM to round
- do not ask the LLM to exponentiate coefficients
- do not ask the LLM to decide how odds should be displayed
- keep raw values available in audit/developer metadata only

### Stage 10.3 - Use model-aware explanation skeletons

Use the Stage 9 rule profile to choose a deterministic explanation skeleton.

Examples:

Numeric GLM:

```text
1. typical/anchor statement
2. change/effect statement
3. uncertainty statement
4. answer sentence
```

Interaction:

```text
1. effect within group/value A
2. effect within group/value B
3. comparison of effects
4. answer sentence
```

Rules:

- structure should be deterministic
- wording can be handled by the LLM
- avoid `interaction term` language
- avoid coefficient decomposition

### Stage 10.4 - Improve response-scale control

Use model profile metadata:

- modelScale
- interpretationScale
- transformationType

Goals:

- avoid log-odds leakage
- avoid log-count leakage
- avoid raw coefficient interpretation
- avoid explaining transformed responses without clear scale handling

Special issue:

The app supports both derived transformed variables and inline transformed formula responses, such as:

```text
log.y ~ x
log(y) ~ x
```

Stage 10 should treat these consistently where possible.

### Stage 10.5 - Improve comparison control

Use `comparisonScope` and rule-profile metadata to prevent comparison explosion.

Default behaviour:

- do not enumerate all treatment comparisons unless the research question asks for it
- for multi-factor models, prefer a global summary where appropriate
- use targeted comparisons only when they are relevant to the research question

Preferred wording examples:

```text
The model suggests that the response is not the same across the treatment groups.
```

```text
There is evidence that the response differs between the treatment groups.
```

### Stage 10.6 - Improve interaction explanations

Interactions should follow the within-group-then-compare structure.

Preferred pattern:

```text
In Group A, a one-unit increase in x changes the expected response by ...
In Group B, the same one-unit increase changes the expected response by ...
This means the effect of a change in x is different between the groups.
```

Avoid:

- interaction term
- coefficient-level explanation
- component-by-component decomposition

### Stage 10.7 - Use validation feedback to guard prompts

Use Stage 9 validation flags to identify prompt failures.

Initial guard targets:

- technical scale leakage
- raw coefficients
- odds displayed as ordinary decimals
- vague effect wording for numeric predictors
- missing answer
- missing uncertainty where required
- excessive comparisons
- interaction not compared

The first implementation should probably flag and surface problems rather than automatically regenerate explanations.

## Testing rules

Use deterministic offline tests.

Do not use real LLM calls in tests.

Use fake providers, existing prompt helpers, mocks, or fixtures.

Tests should check:

- prompt inputs include formatted quantities
- prompt inputs avoid raw log-scale quantities where not appropriate
- skeleton selection works by model family and structure
- interaction prompts use within-group-then-compare structure
- comparison scope prevents exhaustive pairwise comparisons
- legacy behaviours remain intact where Stage 8 examples were already strong

## Coding conventions

Follow the user's standing WMFM conventions:

- use `=` for assignment
- use camelCase identifiers
- use braces for all control structures
- use styleR-style formatting
- prefer `|>` over magrittr
- use roxygen2 documentation
- prefer `@importFrom` rather than `pkg::fun()` where appropriate
- do not manually edit `NAMESPACE`
- keep tests deterministic and offline
- do not make real LLM calls in tests
- provide replacement files or zips rather than long copy-paste patches
- provide plain-text ASCII git commit messages

## Non-goals for early Stage 10 work

Do not redesign:

- explanationAudit
- scoring
- grading
- bad explanation generation
- claim-evidence map internals beyond what is needed for prompt integration

Do not remove legacy fields or break existing developer-feedback objects.

Do not change student-facing UI layout unless specifically requested.

## Recommended first step

Begin with:

```text
Stage 10.1 - Audit current prompt inputs
```

This should inspect the existing prompt path and identify exactly where Stage 9 metadata can be injected safely.

The first code change should be narrow and reversible.
