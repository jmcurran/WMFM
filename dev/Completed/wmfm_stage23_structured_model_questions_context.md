# WMFM Stage 23 context: structured model follow-up questions and prediction intervals

## Stage identity

Stage: 23  
Starting package version: 0.2.0.001  
Versioning rule: increment the build number at each sub-stage, for example:

- 23.1 -> 0.2.0.001
- 23.2 -> 0.2.0.002
- 23.3 -> 0.2.0.003

This workstream extends the earlier "Any other questions?" context, but narrows the design toward a safer structured model-query system rather than an unrestricted prompt-extension box.

## Core design decision

Separate the **research question** from **follow-up/model questions**.

They are related, but they should not be the same UI or internal field.

### Research question

The research question is part of the core explanation context. It describes the main question the fitted model is intended to answer.

It should:

- be short and stable for the current model fit
- shape the standard explanation
- help the final explanation answer the student's main question
- be stored as part of the model/explanation state
- affect explanation cache keys when included in generated text

Examples:

- "Does regular attendance predict exam performance after accounting for test score?"
- "How does earthquake frequency change with magnitude in each location?"
- "What is the predicted exam mark for a regular attender who scored 10 out of 20 on the test?"

A prediction-shaped research question should be recognised as such, but the research question itself should not become an unrestricted chat prompt.

### Follow-up/model questions

Follow-up/model questions are bounded requests made after or alongside the fitted model explanation.

They should:

- be constrained to legitimate model-explanation tasks
- be classified before use
- dispatch deterministic computations where possible
- ask the LLM to narrate computed results, not invent them
- reject or defer unsupported requests clearly
- preserve the standard explanation when empty

Examples:

- "What is the predicted exam mark for attendance = Yes and test = 10?"
- "Give me a prediction interval for that student."
- "Explain the effect of test score for a 10-unit increase."
- "Focus on the comparison between Washington and Southern California."
- "Make the explanation shorter and aimed at beginners."

## Why this split matters

A research question such as:

"What is my predicted exam mark if I attend class regularly and get 10 out of 20 in the test?"

is not just a style instruction. It implies a specific model-derived quantity. For ordinary linear regression, this should usually produce:

- the fitted mean prediction
- a confidence interval for the mean response, if useful
- a prediction interval for an individual outcome, if supported

The app should distinguish:

- confidence interval for the expected mean response
- prediction interval for an individual observation

This is a high-value teaching point. Students often confuse these intervals, and this feature can make the distinction explicit.

## Recommended Stage 23 shape

Approximate number of sub-stages: **9 to 11**.

A sensible first schedule is 10 sub-stages. Some may be split if tests or architecture reveal hidden complexity.

## Stage 23 proposed schedule

### Stage 23.1 - Audit existing explanation, research-question, and CI pathways

Version: 0.2.0.001

Inspect the current code paths before changing behaviour.

Likely files:

- R/app-ui.R
- R/app-server.R
- R/prompt-explain.R
- R/prompt-core.R
- R/model-lm-explanation.R
- R/model-ci-data.R
- R/model-ci-support.R
- R/app-modelOutput-serverHelpers.R
- R/api-runModel.R

Deliverables:

- identify where research question state currently belongs
- identify where explanation cache keys are formed
- identify where confidence-interval payloads are produced
- identify whether prediction intervals are already available anywhere
- confirm whether follow-up questions should be implemented as prompt augmentation, deterministic model-query helpers, or a separate API path

Expected outcome:

- no major feature yet
- small refactor only if required to expose existing state cleanly

### Stage 23.2 - Add explicit research-question state and cache integration

Version: 0.2.0.002

Implement or stabilise the research-question field as a first-class input to the explanation pipeline.

Requirements:

- research question stored in Shiny state
- passed into explanation prompt payload
- included in explanation cache keys when it affects generated text
- empty research question preserves current behaviour
- prompt contract states that the research question guides the explanation but does not override model facts

Tests:

- empty research question gives unchanged standard prompt behaviour
- non-empty research question appears in the prompt payload
- cache key changes when research question changes

### Stage 23.3 - Add bounded follow-up question UI state without changing model answers yet

Version: 0.2.0.003

Add the UI affordance for follow-up/model questions.

Recommended label:

- "Ask about this model"

Avoid labels that imply open-ended general chat.

Requirements:

- expandable panel or button-revealed text box
- text stored in Shiny state
- empty field preserves current explanation behaviour
- follow-up text is not simply pasted into the prompt as an unconstrained instruction

Tests:

- UI contains the new input
- server state records the input
- empty follow-up produces no prompt change

### Stage 23.4 - Implement follow-up request classification and guardrails

Version: 0.2.0.004

Create a deterministic classifier for supported request categories.

Initial categories:

- style_or_audience
- concise_summary
- prediction_request
- prediction_interval_request
- alternative_unit_change
- subgroup_or_factor_comparison
- unsupported_or_out_of_scope

Requirements:

- classification is deterministic and conservative
- unsupported requests are not passed through as free-form LLM instructions
- prompt-injection-like requests are treated as out of scope
- classifier output is included in the prompt payload only as bounded metadata

Tests:

- representative examples classify correctly
- unsafe/unrelated examples classify as unsupported
- mixed requests are either split safely or marked as needing deterministic support

### Stage 23.5 - Add deterministic linear-model prediction helper

Version: 0.2.0.005

Support ordinary linear-model prediction requests for explicit covariate values.

Requirements:

- parse or receive structured prediction inputs
- compute fitted mean response deterministically
- compute standard error and confidence interval for the mean response where supported
- return structured result object
- do not rely on the LLM to calculate model-derived values

Important design point:

Free-text parsing can be fragile. The first version may use a semi-structured UI for variable values, or may support only simple detected requests while clearly deferring ambiguous ones.

Tests:

- known lm predictions match stats::predict()
- confidence intervals match stats::predict(..., interval = "confidence")
- unsupported models fail gracefully

### Stage 23.6 - Add deterministic linear-model prediction intervals

Version: 0.2.0.006

Add prediction intervals for individual outcomes in ordinary linear regression.

Requirements:

- compute prediction interval via stats::predict(..., interval = "prediction") or equivalent
- clearly label prediction interval versus confidence interval
- include both fitted mean and interval bounds in structured payload
- explain that prediction intervals are wider because they include individual variation around the fitted mean

Tests:

- prediction intervals match stats::predict(..., interval = "prediction")
- prediction interval is wider than confidence interval in standard cases
- prompt payload contains both interval type and numeric values
- no prediction interval is claimed for unsupported model families

### Stage 23.7 - Add research-question-aware prediction behaviour

Version: 0.2.0.007

When the research question is clearly prediction-shaped, the app should trigger or recommend the prediction-oriented explanation path where the required covariate values are available.

Requirements:

- detect individual prediction wording conservatively
- if enough covariate values are available, compute prediction result
- if values are missing, explain what inputs are needed
- for lm models, include prediction interval when appropriate
- do not replace the core explanation entirely unless that is the intended UI mode

Tests:

- prediction-shaped research question triggers prediction payload
- association-shaped research question keeps standard explanation behaviour
- missing covariates produce a clear message rather than invented values

### Stage 23.8 - Integrate bounded follow-up results into LLM narration

Version: 0.2.0.008

Update the prompt contract so the LLM narrates structured follow-up results.

Requirements:

- LLM must use supplied deterministic values
- LLM must not compute unsupported quantities
- LLM must distinguish confidence intervals from prediction intervals
- standard explanation remains stable when no follow-up exists
- unsupported requests produce a polite bounded response

Tests:

- prompt contains deterministic follow-up payload
- prompt rules prohibit invented calculations
- generated prompt includes clear interval distinction
- empty follow-up produces current standard prompt

### Stage 23.9 - Add alternative-unit and comparison follow-up support

Version: 0.2.0.009

Support selected non-prediction follow-up categories.

Initial supported examples:

- interpret a numeric slope for a 10-unit increase
- focus on a particular factor comparison already present in the model
- make the explanation shorter or aimed at beginners

Requirements:

- deterministic computation for transformed numeric effects where needed
- no LLM-only numeric transformations unless the value is already supplied
- unsupported transformations are clearly declined or deferred

Tests:

- 10-unit lm slope interpretation is computed deterministically
- multiplicative GLM interpretations are handled only where existing infrastructure supports them
- style-only requests alter narration but not model facts

### Stage 23.10 - UI polish, documentation, and regression tests

Version: 0.2.0.010

Stabilise Stage 23 behaviour.

Requirements:

- UI language is educational and not chat-like
- documentation/comments explain research question versus follow-up question
- tests cover empty fields, supported requests, unsupported requests, cache behaviour, and prediction intervals
- package documentation regenerated
- version incremented correctly

Expected final behaviour:

- research question guides the core model explanation
- follow-up/model questions provide bounded, model-aware extensions
- linear-model prediction questions can return fitted prediction and prediction interval
- unsupported requests do not derail the app or produce invented calculations

## Possible optional Stage 23.11 - GLM prediction interval policy

Version: 0.2.0.011

This should probably be deferred unless Stage 23.1 shows that the current infrastructure already makes it easy.

Reason:

Prediction intervals for GLMs are more nuanced than ordinary lm prediction intervals. For logistic regression, prediction is often on the probability scale, but an individual binary outcome interval is not analogous to an lm prediction interval. For Poisson models, predictive distributions may be possible, but they require careful family-specific handling.

Recommended policy for first version:

- lm: support prediction intervals
- logistic: support predicted probabilities and confidence intervals for expected probability, but do not call them prediction intervals
- Poisson: support expected count and confidence interval where already supported; defer full predictive intervals unless deliberately implemented
- unsupported models: explain limitation clearly

## Guardrails

The follow-up feature must not become an unrestricted general chat interface.

Rules:

- do not let follow-up text override WMFM explanation rules
- do not let follow-up text override model results
- do not invent variables, predictions, intervals, or transformations
- classify before prompting
- compute model-derived quantities deterministically
- use the LLM for narration only after deterministic values are supplied
- decline unsupported requests plainly
- preserve standard explanation behaviour when follow-up input is empty

## Suggested implementation architecture

Prefer three layers:

1. UI/state layer

- researchQuestion
- followUpQuestion or modelQuestion
- optional structured inputs for prediction requests

2. deterministic model-query layer

Possible helper names:

- classifyModelQuestion()
- buildModelQuestionRequest()
- computeLmPredictionSummary()
- computeLmPredictionInterval()
- buildModelQuestionPayload()

3. prompt narration layer

Possible helper names:

- buildResearchQuestionPromptBlock()
- buildModelQuestionPromptBlock()
- buildModelQuestionNarrationRules()

The LLM prompt should receive:

- original model explanation payload
- research question block
- optional classified follow-up block
- optional deterministic result payload
- explicit limitations or unsupported-request message

## Testing priorities

High-value tests:

- research question appears only when supplied
- explanation cache key changes with research question and follow-up question
- empty follow-up preserves existing prompt
- classifier recognises prediction and prediction interval requests
- unsupported requests are blocked
- lm confidence interval matches stats::predict()
- lm prediction interval matches stats::predict()
- LLM prompt distinguishes mean-response CI from prediction interval
- version number increments at each sub-stage

## Plain-language teaching target

When prediction intervals are available, the explanation should be able to say something like:

"The fitted model predicts an exam mark of about 49 for a regular attender with a test score of 10. The confidence interval describes uncertainty in the average mark for students with those values. The prediction interval is wider because individual students vary around that average; it gives a more realistic range for one student's actual mark."

## Notes for future chats

Use the user's usual WMFM working conventions:

- R identifiers in camelCase
- use = for assignment
- braces on all control structures
- roxygen2 documentation
- prefer @importFrom over pkg::fun()
- keep app-server.R thin
- separate helper logic where sensible
- update generated documentation when roxygen changes
- provide downloadable changed files or zip files
- provide plain-text git commit messages with no unicode
- do not claim local R validation unless R actually ran
- use GitHub Actions as authoritative when local R is unavailable

## Open design question to revisit at Stage 23.1

Should prediction inputs be captured through:

1. free text only
2. semi-structured variable-value UI
3. both, with free text used only to classify the request and structured UI used for computation

Initial recommendation:

Use both eventually, but start conservatively. Free text can identify that the user wants a prediction; structured inputs should provide the actual covariate values used for deterministic computation.
