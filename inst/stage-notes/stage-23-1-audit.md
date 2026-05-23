# Stage 23.1 Audit: Follow-up Question Architecture

## Scope
This audit inspected the current explanation, research-question, confidence-interval, prediction, and prompt pathways without implementing Stage 23 follow-up-question features.

Inspected files:
- `R/app-ui.R`
- `R/app-server.R`
- `R/prompt-explain.R`
- `R/prompt-core.R`
- `R/model-lm-explanation.R`
- `R/model-ci-data.R`
- `R/model-ci-support.R`
- `R/app-modelOutput-serverHelpers.R`
- `R/api-runModel.R`

---

## Audit findings by question

### 1) Where should the research question live in Shiny state?
- UI already captures it as `input$researchQuestion` in the Model tab.
- Best current architecture is:
  - **source-of-truth at UI input** during editing;
  - **copied onto fitted model object** at fit time as `attr(model, "wmfm_research_question")`;
  - **read from model attribute** for explanation and cache key generation.
- This avoids drift between displayed model and explanation context.

### 2) Is there already a research-question field or related prompt input?
- Yes.
- `R/app-ui.R` defines a dedicated `textInput("researchQuestion", ...)` with teaching guidance text.
- Prompt builders consume `wmfm_research_question` when present.

### 3) Where are explanation prompts assembled?
- Main explanation prompt assembly is in `lmToExplanationPrompt()` (`R/prompt-explain.R`).
- Shared language contract and prompt composition are in `R/prompt-core.R` via:
  - `getResearchQuestionGuidanceLines()`
  - `buildWmfmLanguageContractText()`
  - `composeWmfmPrompt()`

### 4) Where are explanation cache keys built?
- In `buildLmExplanationCacheKey()` (`R/model-lm-explanation.R`).
- Called from `lmExplanation()` before LLM call.

### 5) Which inputs currently affect the cache key?
`buildLmExplanationCacheKey()` currently includes:
- model formula text (`formulaStr`)
- coefficient payload text (`coefStr`)
- numeric anchor cache token (`numericAnchorCacheKey`)
- research question text (trimmed)
- adjustment variable set (`adjustmentVariables`, sorted/unique)
- hard-coded policy version tag.

Conclusion: research question already affects explanation cache invalidation, which is aligned with Stage 23 direction.

### 6) Where are confidence interval payloads generated?
- High-level CI payload generator: `buildModelConfidenceIntervalData()` (`R/model-ci-data.R`).
- It chooses between derived teaching rows and coefficient fallback using `classifyModelConfidenceIntervalPlan()`.
- Row-level fitted-quantity interval construction uses deterministic helpers such as:
  - `addFittedQuantityRows()`
  - `buildConfidenceIntervalPrediction()`
- UI rendering/filtering for CI tables is in `registerModelOutputTabs()` (`R/app-modelOutput-serverHelpers.R`).

### 7) Is there any existing prediction helper or prediction interval support?
- There is an internal helper named `buildConfidenceIntervalPrediction()` but it computes intervals for fitted/link-scale means, not observation-level prediction intervals.
- `computeMeanCi()` (`R/model-ci-support.R`) explicitly documents mean/fitted-response confidence intervals and explicitly distinguishes from prediction intervals.
- No dedicated `lm` prediction-interval pathway (e.g., `interval = "prediction"`) is currently exposed.
- No GLM family-specific prediction-interval framework is present.

### 8) How are lm, logistic, and Poisson model explanation quantities currently separated?
- Separation occurs via model-family checks and scale-specific blocks:
  - `lmToExplanationPrompt()` sets model/outcome descriptors by family/link.
  - CI builders and row constructors branch by identity (lm), binomial-logit, and poisson-log support.
  - Logistic quantities are represented as probabilities + odds-derived effect scales.
  - Poisson quantities are represented as expected values + multiplicative expected-count effects.

### 9) Safest future architecture for bounded follow-up/model questions
Recommended architecture (Stage 23.2+):
1. **Separate state objects**
   - `researchQuestion` remains core explanation context.
   - New `followupQuestion` state is independent and ephemeral.
2. **Bounded request classifier**
   - Deterministic classifier maps follow-up input into finite intents (e.g., `clarify_effect`, `compare_levels`, `predict_mean`, `predict_individual_lm`, `ci_explain`, `unsupported`).
3. **Deterministic-first dispatcher**
   - For supported intents, run deterministic computation helpers first.
   - Only pass structured computed results to LLM for narration.
4. **Strict prompt boundary**
   - LLM never sees raw app-control instructions from follow-up text.
   - Follow-up text is treated as data, sanitized, and embedded only in bounded templates.
5. **Policy gate + safe fallback**
   - If intent unsupported/ambiguous/injection-like, return fixed safe guidance and suggested rephrase.
6. **Cache partitioning**
   - Keep explanation cache independent from follow-up answer cache.
   - Follow-up cache key should include intent class + deterministic payload hash + rendering policy version.

### 10) Deterministic computation vs LLM narration
- **Deterministic should own**:
  - model-derived numeric quantities (fitted means, odds ratios, expected-count multipliers, confidence intervals)
  - comparison calculations and uncertainty transforms
  - future lm prediction intervals (individual outcomes)
  - intent classification and policy checks.
- **LLM should own**:
  - plain-language narration/teaching phrasing of deterministic outputs
  - concise uncertainty wording consistent with WMFM rules.
- **LLM should not own**:
  - numerical derivation, unsupported feature claims, or open-ended chat behavior.

---

## Stage 23.2+ likely file-change list
- `R/app-ui.R` (add bounded follow-up input UI)
- `R/app-server.R` (wire state + observers)
- `R/prompt-core.R` (bounded follow-up narration templates/contracts)
- `R/prompt-explain.R` (ensure separation from follow-up pathway)
- `R/model-lm-explanation.R` (cache boundary / policy versioning)
- `R/model-ci-data.R` (reuse deterministic quantities for follow-up intents)
- `R/model-ci-support.R` (potential new lm prediction-interval helper)
- `R/app-modelOutput-serverHelpers.R` (display integration)
- `R/api-runModel.R` (optional API entry points for bounded questions)
- likely new files, e.g.:
  - `R/followup-classify.R`
  - `R/followup-dispatch.R`
  - `R/followup-policy.R`
  - `R/followup-predict.R`

## Stage 23.2+ test plan (to add)
1. **Classifier tests (deterministic/offline)**
   - intent mapping for canonical question forms
   - injection-like text classification to safe fallback
   - ambiguous question handling.
2. **Dispatcher tests**
   - each intent routes to expected deterministic helper
   - unsupported intents return bounded response.
3. **Prediction tests (`lm`)**
   - distinguish confidence interval for mean vs prediction interval for individual.
   - verify numeric equality against `predict.lm(..., interval=...)` references.
4. **GLM guardrail tests**
   - ensure no unsupported prediction-interval promises.
5. **Prompt-boundary tests**
   - follow-up text cannot override WMFM instruction contract.
6. **Cache-key tests**
   - research-question changes invalidate explanation cache.
   - follow-up cache key isolated from base explanation cache.
7. **Shiny state tests**
   - research question persistence through fit/reset paths.
   - follow-up state reset behavior.

## Minimal code-change note for Stage 23.1
- No functional code-path changes were made in this audit stage.
- Deliverable is documentation-only to confirm architecture before implementation.

## Validation status
Validation commands were attempted after this audit file was added.
