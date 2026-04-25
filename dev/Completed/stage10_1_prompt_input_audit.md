# Stage 10.1 - Prompt input audit

## Scope

This audit maps the current WMFM model-explanation prompt path in the completed Stage 9 codebase. It is intentionally narrow: it identifies where raw or partly processed values enter the explanation prompt and where Stage 9 deterministic metadata can be injected later.

No broad prompt rewrite is proposed in this step.

## Current explanation-generation path

1. `runModel()` fits the model and builds deterministic Stage 9 objects.
   - File: `R/api-runModel.R`
   - Builds `explanationAudit = buildModelExplanationAudit(model = model)`.
   - Builds `modelProfile = buildExplanationModelProfile(model = model, data = dataModel, modelType = modelType)`.
   - Calls `lmExplanation(model = model, chat = chatProvider, useCache = useExplanationCache)` when a chat provider is available.

2. `buildAppModelOutputs()` follows the same app-side pattern.
   - File: `R/app-equation-runtime.R`
   - Calls `buildAppExplanation()`, which calls `lmExplanation()`.
   - Calls `buildAppExplanationAudit()` separately.

3. `lmExplanation()` handles caching and calls the LLM.
   - File: `R/model-lm-explanation.R`
   - Cache key currently includes formula, coefficients, numeric-anchor cache key, and research question.
   - Builds `prompt = lmToExplanationPrompt(model)`.
   - Calls `chat$chat(prompt)`.
   - Applies `normaliseNumericExpressions(output)` after generation.

4. `lmToExplanationPrompt()` builds the main prompt payload.
   - File: `R/prompt-explain.R`
   - Constructs model description, outcome description, dataset block, research question block, numeric-anchor block, anchored baseline block, coefficient table, and confidence-interval table.
   - Sends all of this to `composeWmfmPrompt(context = "summary", contextPayload = contextPayload)`.

5. `composeWmfmPrompt()` prepends the shared language contract.
   - File: `R/prompt-core.R`
   - Adds `buildWmfmLanguageContractText(context = "summary")` before the context payload.
   - Ends with `Write the explanation now.`

6. After LLM output, Stage 9 deterministic tools are applied downstream.
   - File: `R/api-runModel.R`
   - Cleans text with `cleanExplanationText()`.
   - Builds teaching summary with `buildExplanationTeachingSummary()`.
   - Builds claim/evidence map with `buildExplanationClaimEvidenceMap()`.
   - Validation and quality flags are available via Stage 9 objects, but they currently guard or diagnose output rather than shaping prompt inputs.

## Current prompt inputs

### Plain model metadata

Source: `R/prompt-explain.R`

- Response variable name from `model.frame(model)`.
- Number of observations.
- Linear-model R-squared if available.
- Model description string such as `generalised linear model with poisson family and log link`.
- Outcome description string for continuous, binary, or count outcomes.
- Optional dataset documentation block.
- Optional research-question block.

Audit status:

- Much of this overlaps with `buildModelExplanationAuditOverview()` and `buildModelExplanationAuditPromptInputs()`.
- The LLM still receives model-family/link language, even though the language contract asks it not to expose this unless needed.

### Numeric anchors

Source: `R/prompt-explain.R` and `R/model-explanationAnchor.R`

- `buildModelNumericAnchorInfo()` returns `numericAnchorInfo$promptText` and `numericAnchorInfo$cacheKey`.
- The prompt tells the LLM to use the chosen anchor rather than defaulting to 0.

Audit status:

- Anchor metadata also appears in `explanationAudit$numericAnchor`.
- Anchor values should be passed in Stage 9 formatted form using `formatExplanationAnchor()` rather than relying on generic rounding text.

### Anchored baseline fitted values

Source: `R/prompt-baselineSummary.R`

- `buildAnchoredBaselinePromptBlock()` uses `buildModelConfidenceIntervalData()` and selects baseline rows.
- For Poisson models, it selects expected-value rows.
- For binomial models, it selects odds rows.
- It formats estimates and confidence limits with `formatConfidenceIntervalNumber()`.

Audit status:

- This is already a useful deterministic block.
- It does not yet use the Stage 9 explanation-formatting helpers in `R/model-explanationFormat.R`.
- Binomial baselines are odds only, while Stage 10 may need clear probability/odds control depending on the explanation rule profile.

### Coefficient table

Source: `R/prompt-explain.R`

- `coef(summary(model))` is rounded to 4 decimals and printed directly into the prompt.

Risk:

- For logistic models, this exposes log-odds coefficients.
- For Poisson models, this exposes log-count coefficients.
- For transformed responses, this exposes model-scale quantities.
- The language contract tells the LLM to convert these values, but the LLM is still being given raw coefficient-scale inputs.

Stage 10 action target:

- Keep raw coefficients in `explanationAudit$coefficientTable` or developer metadata only.
- Replace the student-facing prompt block with deterministic, formatted effect summaries from Stage 9 confidence-interval/evidence helpers.

### Confidence interval table

Source: `R/prompt-explain.R`

- `confint(model)` is rounded to 4 decimals and printed directly into the prompt.

Risk:

- For logistic models, intervals are on the log-odds coefficient scale.
- For Poisson models, intervals are on the log coefficient scale.
- For transformed responses, intervals are on the model scale.
- The LLM is being asked to infer how to convert these intervals into student-facing interpretations.

Stage 10 action target:

- Use `buildModelConfidenceIntervalData()` output instead of raw `confint(model)` in the prompt.
- Prefer formatted estimate/lower/upper text using `formatExplanationConfidenceInterval()`, `formatExplanationOdds()`, `formatExplanationProbability()`, and `formatExplanationMultiplier()` as appropriate.

### Model profile and rule profile

Source: `R/api-runModel.R`, `R/model-explanationProfile.R`, and `R/model-explanationRules.R`

- `runModel()` builds `modelProfile` and stores it in the returned `wmfmModel`.
- `buildExplanationRuleProfile()` can derive:
  - `skeletonId`
  - `skeletonSteps`
  - `comparisonScope`
  - `comparisonGuidance`
  - `scaleGuidance`
  - `effectLanguage`
  - `avoidTerms`
  - `qualityFlagsToCheck`

Current gap:

- `lmToExplanationPrompt()` does not currently build or receive a rule profile.
- Skeleton steps and comparison guidance are not yet included in the explanation prompt.

Stage 10 action target:

- Add a deterministic prompt block derived from `buildExplanationRuleProfile(buildExplanationModelProfile(...))`.
- Keep this block as structure/guidance, not prose that the LLM can override.

### Validation feedback

Source: `R/model-explanationValidation.R` and claim/evidence mapping helpers

- Stage 9 can flag quality issues after explanation text exists.
- Current explanation generation does not use those flags before or during prompt construction.

Current gap:

- Validation is diagnostic, not a prompt guard.

Stage 10 action target:

- First implementation should surface prompt-relevant guard targets in developer metadata or prompt diagnostics.
- Do not automatically regenerate explanations in the early Stage 10 work.

## Raw or partially processed quantities to replace in Stage 10

Highest priority:

- Raw coefficient table from `coef(summary(model))` in `lmToExplanationPrompt()`.
- Raw confidence intervals from `confint(model)` in `lmToExplanationPrompt()`.
- Family/link model description in the student prompt payload, at least for ordinary cases where scale guidance is enough.
- Any baseline/effect values formatted with generic CI helpers instead of Stage 9 explanation-format helpers.

Medium priority:

- Numeric anchor text that embeds unformatted or over-precise values.
- Outcome-scale wording for transformed responses, especially consistency between `log.y ~ x` and `log(y) ~ x`.
- Research-question instructions that may encourage exhaustive answers when comparison scope should be minimal or targeted.

Keep as developer/audit metadata:

- Raw coefficient table.
- Raw confidence interval values.
- Link/family details.
- Model-scale transformation details.
- Full prompt ingredients for diagnostics.

## Safe injection points for Stage 10.2+

1. Add a prompt-side deterministic summary helper.
   - Candidate file: new `R/prompt-explanationInputs.R`, or a targeted addition to `R/prompt-explain.R`.
   - Purpose: build formatted student-ready prompt blocks from Stage 9 evidence tables.

2. Add a rule-profile prompt block.
   - Candidate source: `buildExplanationRuleProfile()`.
   - Include skeleton steps, scale guidance, comparison guidance, effect language, and avoid terms.

3. Replace raw prompt tables gradually.
   - First keep raw coefficient and CI tables in the prompt only behind a temporary internal/debug switch, or remove them once formatted evidence blocks are covered by tests.
   - Update cache key when prompt inputs change.

4. Extend tests offline.
   - Add prompt tests that inspect the prompt string produced by `lmToExplanationPrompt()`.
   - Check that logistic prompts avoid raw log-odds coefficients and include formatted odds/probability guidance as intended.
   - Check that Poisson prompts avoid raw log-count intervals and include formatted expected-count multipliers.
   - Check that interaction prompts include within-group-then-compare skeleton guidance.
   - Check that factor prompts include minimal/targeted comparison guidance.

## Recommended next code change

Stage 10.2 should begin with a narrow helper that converts `buildModelConfidenceIntervalData()` into a formatted prompt block.

Suggested shape:

- `buildExplanationPromptQuantityBlock(model, mf = NULL, predictorNames = NULL)`
- Returns a character scalar suitable for the prompt.
- Uses `buildModelConfidenceIntervalData()` for deterministic quantities.
- Uses `formatExplanationQuantity()` and related helpers for student-ready text.
- Leaves raw coefficients and raw `confint(model)` available only in `buildModelExplanationAudit()` and developer metadata.

This change is reversible because it can initially be added alongside the existing prompt tables and tested before the raw blocks are removed.
