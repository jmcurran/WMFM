# Stage 47.1: Intelligent question handling audit and design

## Status

Stage 47.1 is an audit-and-design stage. It does not change user-facing question handling.

Authoritative baseline:

```text
stage46.1.4_completed.zip
```

Development branch:

```text
stage-47-intelligent-question-handling
```

Version series:

```text
1.1.3.xxx
```

Stage 47.1 uses version `1.1.3.001`.

## Objective

Stage 47 will make WMFM respond helpfully when a question is not a valid request for interpretation of the fitted model. The intended behaviour is not simply to reject unusual questions. WMFM should distinguish among questions that:

- can be answered from the fitted model;
- can be answered only after obtaining missing predictor values or a missing definition;
- require a different response variable, model, or statistical analysis;
- ask for general statistical or educational explanation;
- ask why the analysis is being performed or why a prediction is being made;
- are too vague to interpret safely; or
- are not meaningful questions.

The central design principle is that WMFM must not continue with an apparently authoritative model interpretation when the user's actual question cannot be answered by the current analysis.

## Executive findings

The current code base already contains a substantial and effective bounded follow-up framework. It is not necessary to design a new universal intent-classification subsystem from scratch.

The main gap is narrower:

1. the mandatory research question is mostly treated as framing text rather than classified for answerability;
2. the optional follow-up question has detailed classification, but unmatched questions collapse into one broad `unsupported_or_out_of_scope` category;
3. unsupported follow-up text is safely withheld from the LLM, but there is usually no dedicated student-facing response explaining why the question is unsupported or what to do instead;
4. educational, purpose, clarification, missing-definition, and alternative-analysis questions are not distinguished;
5. research-question and follow-up-question routing are not represented by one shared contract.

Stage 47 should therefore extend and generalise the existing bounded routing architecture rather than replace it.

## Current question entry points

### Mandatory research question

The Model tab requires `researchQuestion` before fitting. The server stores it in:

```r
attr(model, "wmfm_research_question")
```

The text is used to frame the explanation and is passed into the explanation prompt. It is not normally classified for validity, relevance, or answerability.

One exception exists: `buildResearchQuestionPredictionPayload()` recognises prediction-shaped research questions. It attempts a deterministic prediction without completing omitted predictors. Missing predictor values are represented as a `needs_input` prediction result.

This is useful but narrow. Questions such as these are not currently routed at research-question entry:

```text
Why are you predicting my exam result?
Why are we doing this?
I don't know.
What is regression?
What is the chance I will pass?
```

### Optional follow-up question

The Model tab also provides `modelFollowupQuestion`. It is classified by:

```r
classifyModelFollowupQuestion()
```

The resulting payload is enriched deterministically for supported computations and stored in:

```r
attr(model, "wmfm_model_followup_payload")
```

This is the strongest existing foundation for Stage 47.

### Programmatic interface

`runModel()` accepts both a research question and an optional follow-up question. It uses the same follow-up classifier and deterministic enrichers. Stage 47 must preserve parity between the Shiny application and this programmatic route.

### Example loading and retained state

Examples can populate a research question. Application state retains both research and follow-up text. Any new routing result must be refreshed whenever the corresponding text, data, response, formula, or fitted model changes.

## Existing follow-up classification

The existing classifier recognises the following broad capabilities.

### Deterministic statistical requests

- `prediction_request`
- `prediction_interval_request`
- `unit_change_request`
- `proportional_change_request`
- `observation_residual_request`
- `comparable_observation_request`
- `adjustment_prediction_comparison`

These are already supported by deterministic computation or bounded deterministic guidance.

### Explicit statistical boundary

- `conditional_quantile_request`

This is currently classified as unsupported and can carry a deterministic explanation that an ordinary conditional-mean model does not provide the requested conditional distribution. This is the closest existing example of the desired Stage 47 behaviour.

### Explanation preferences

- `emphasis_uncertainty`
- `emphasis_effect_size`
- `emphasis_practical_interpretation`
- `emphasis_group_comparison`
- `emphasis_interaction`
- `beginner_friendly`
- `concise_answer`
- `focus_research_question`

These are not distinct statistical questions. They modify how the main explanation is presented.

### Unsupported fallback

Everything else defaults to:

```text
unsupported_or_out_of_scope
```

The same category also covers prompt injection, unrelated creative requests, ambiguous unit changes, and ordinary unmatched questions. This is safe but not sufficiently informative for Stage 47.

## Current safety behaviour

The present framework already has several important safeguards that Stage 47 should retain.

- Raw unsupported follow-up text is withheld from the LLM.
- User text is treated as bounded context rather than a free-form instruction.
- Deterministic predictions and residual rankings remain the authoritative numeric result.
- The LLM is instructed not to invent missing predictor values, thresholds, intervals, percentiles, cases, or computations.
- Extrapolation can block deterministic prediction and speculative LLM prediction.
- Prediction-shaped research questions do not silently complete missing predictors.

The problem is therefore not uncontrolled prompting. The problem is that the resulting student-facing response does not yet consistently address the reason the question failed.

## Characterisation of unusual questions

The following table records the expected current route from source inspection and the preferred Stage 47 route. Stage 47.2 should convert these into executable characterisation tests before behaviour changes.

| Question | Current effective route | Preferred route |
|---|---|---|
| `Why are you predicting my exam result?` | Unsupported follow-up, or unclassified research framing | Purpose or application explanation |
| `Why are we doing this?` | Unsupported follow-up, or unclassified research framing | Clarification or purpose explanation |
| `I don't know.` | Unsupported follow-up, or accepted mandatory research text | Unclear or non-question; offer question guidance |
| `What is regression?` | Unsupported follow-up, or unclassified research framing | General statistical education |
| `What is logistic regression?` | Unsupported follow-up, or unclassified research framing | Model-specific statistical education |
| `Will I pass the course?` | May be prediction-like only when recognised by prediction patterns; otherwise unsupported | Capability check followed by missing definition or alternative analysis |
| `What is the chance I will pass?` | Usually lacks a complete deterministic prediction condition | Missing definition and/or alternative response-model guidance |
| `Is this a good model?` | Unsupported | Diagnostic or model-adequacy route, with explicit limits |
| `Tell me something interesting.` | Unsupported | Clarification request with examples of answerable questions |
| `Does attendance cause better exam results?` | Unclassified research framing or unsupported follow-up | Causal-claim boundary; explain observational association limits |
| `What should I ask?` | Unsupported | Capability guidance based on the current model |

## The central distinction: answerability, not topic alone

A question can mention the response and predictors while still being unanswerable by the fitted model.

For example, with:

```r
Exam ~ Attend + Test
```

`What mark is predicted when Attend = 90 and Test = 75?` is answerable if the supplied values are valid.

`Will I pass?` is not automatically answerable because it may lack:

- predictor values for the individual;
- a definition of the pass threshold;
- an appropriate individual-outcome target; and
- for a probability of passing, a model for a binary pass/fail response or a justified distributional calculation.

Stage 47 should not infer these missing elements. It should identify the precise boundary and give the smallest useful next step.

## Proposed routing contract

Introduce a small shared routing object for both research and follow-up questions. A working name is:

```r
wmfmQuestionRoute
```

This need not initially be a formal S3 class, but it should have a stable named-list contract.

Recommended fields:

```r
list(
  originalText = "...",
  normalizedText = "...",
  source = "research_question" | "followup_question",
  route = "...",
  status = "answerable" | "needs_input" | "unsupported" | "needs_clarification",
  supported = TRUE | FALSE,
  requiresModel = TRUE | FALSE,
  requiresDeterministicComputation = TRUE | FALSE,
  reason = "stable_reason_code",
  missingInformation = character(0),
  recommendedCapability = NULL,
  deterministicResponse = NULL,
  existingPayload = NULL
)
```

`existingPayload` allows current prediction, residual, comparable-observation, unit-change, and adjustment-comparison payloads to remain intact.

## Proposed route taxonomy

The taxonomy should remain deliberately small. Routes describe what WMFM should do; reason codes provide detail.

### `model_answer`

The current fitted model and deterministic machinery can answer the question.

This route wraps existing supported categories rather than duplicating them.

### `explanation_preference`

The question changes style or emphasis rather than asking for a new result.

This wraps the existing concise, beginner, uncertainty, effect-size, practical, comparison, interaction, and research-focus categories.

### `statistical_education`

The user asks what a method or concept means. The response should be educational and should not pretend to derive an answer from the fitted model.

Possible reason codes include:

```text
definition_regression
definition_logistic_regression
definition_confidence_interval
definition_prediction_interval
```

### `analysis_purpose`

The user asks why the analysis or prediction is being performed. The response should use available application context and the stored research purpose, but should not generate a fresh model interpretation.

### `needs_input`

The current analysis could answer after specified information is supplied.

Possible missing information includes predictor values, factor levels, a target observation, or a user-defined threshold.

### `alternative_analysis_needed`

The substantive question is meaningful but requires a different response variable, model, diagnostic, or analysis.

Examples include a pass/fail probability when only a continuous exam-mark model has been fitted, a causal question from observational association, or a conditional percentile from an ordinary mean model.

### `needs_clarification`

The text is too vague or not a usable question. The response should ask one focused clarification or provide a small set of model-specific example questions.

### `out_of_scope`

The request is unrelated, adversarial, or inappropriate for this pathway. Existing prompt-injection and unrelated-request safeguards belong here.

## Reason codes

Stable reason codes should be more specific than route labels. Initial codes should include at least:

```text
missing_predictor_values
missing_outcome_threshold
response_does_not_match_question
requires_binary_response_model
requires_conditional_distribution
requires_diagnostic_assessment
causal_claim_not_supported
unclear_question
not_a_question
unsupported_freeform_instruction
unrelated_request
```

The UI response should be generated from route plus reason, not from fragile matching against the original wording.

## Response ownership

### Entirely deterministic responses

Use deterministic responses when WMFM knows the exact boundary or missing information. Examples:

- missing fitted-model predictors;
- missing pass threshold;
- unsupported extrapolation;
- conditional quantile not supplied by the model;
- binary outcome needed for a direct pass/fail probability;
- causal conclusion not supported by the fitted observational model.

### Deterministic scaffold with optional LLM phrasing

Use this only where natural wording benefits from context, while preserving a deterministic conclusion. Examples:

- why the current analysis is being performed;
- explaining why a different model is more appropriate;
- suggesting answerable questions for the current model.

The prompt must contain the deterministic response contract and must not ask the LLM to decide whether the model can answer.

### Constrained educational LLM response

A dedicated educational route may use the LLM for definitions or conceptual explanation. It should receive only the relevant model type and educational topic, not the full model-interpretation prompt unless that context is genuinely useful.

### Clarification response

For vague text such as `I don't know` or `Tell me something interesting`, WMFM should not attempt analysis. It should ask one focused question or show a short list of relevant examples derived from current model capabilities.

## Recommended architecture

### Preserve the existing classifier

Do not replace `classifyModelFollowupQuestion()` in the first implementation stage. It has broad test coverage and supports mature deterministic pathways.

### Add a higher-level router

Add a wrapper such as:

```r
routeModelQuestion(
  question,
  source,
  model = NULL,
  researchQuestion = NULL
)
```

Suggested order:

1. normalise and validate the text;
2. detect non-question, purpose, education, causal, and broad capability intents;
3. delegate bounded statistical follow-ups to `classifyModelFollowupQuestion()`;
4. inspect the existing payload for missing input or unsupported computation;
5. assess model capability and response compatibility;
6. produce one shared route object;
7. render an appropriate deterministic or constrained response.

Specific statistical requests should retain precedence over broad keywords. For example, a question containing `confidence interval` may be an educational definition or a request to emphasise uncertainty; context and question form must distinguish them.

### Route before assembling the main explanation prompt

Research-question routing must happen before WMFM assumes that a conventional model explanation is the correct response.

The follow-up route can continue to be assembled during fitting, but a dedicated student-facing route response should be rendered separately from the main LLM explanation when the route is not `model_answer` or `explanation_preference`.

### Separate classification from response rendering

Use separate functions, for example:

```r
routeModelQuestion()
buildQuestionRouteResponse()
buildQuestionRoutePromptBlock()
```

The classifier should return evidence and reason codes. It should not directly assemble long UI text.

## User-interface design

### Research question

The research question remains mandatory, but Stage 47 should validate its route before or during fitting.

Recommended behaviour:

- `model_answer`: proceed normally;
- `needs_input`: retain the fitted model if useful, but show exactly what is missing;
- `statistical_education` or `analysis_purpose`: answer without pretending this is the fitted model's research question, and invite a model-oriented question;
- `alternative_analysis_needed`: explain the mismatch before presenting any conventional interpretation;
- `needs_clarification`: do not silently accept the text as sufficient research framing.

A later design decision is needed on whether non-model research questions should block fitting or allow fitting while suppressing the normal explanation. The conservative first implementation should avoid blocking model fitting, but should prevent an irrelevant model explanation from being presented as the answer.

### Follow-up question

Unsupported follow-ups should receive a visible response beneath the main explanation. The response should state:

1. what WMFM understood;
2. why the current model cannot answer it;
3. what information or alternative analysis is needed; and
4. one useful next question where appropriate.

### Capability guidance

Capability examples should be generated from the fitted model and its existing deterministic pathways. Avoid a static universal list that suggests features unavailable for the current model.

## Testing strategy

Stage 47 should remain fully testable offline.

### Characterisation tests first

Stage 47.2 should add a table-driven unusual-question corpus without changing behaviour. Each case should record:

- source: research or follow-up;
- fitted model family;
- question text;
- current category;
- proposed route;
- proposed reason code;
- required response ownership.

### Classifier unit tests

Test route and reason separately. Avoid asserting full prose except for short deterministic boundary messages.

### Model capability tests

Use small offline models covering:

- ordinary linear regression;
- logistic regression;
- Poisson regression;
- missing predictor values;
- absent factor levels;
- transformed predictors and responses;
- factor-only and numeric-predictor models;
- prediction and extrapolation boundaries.

### Prompt safety tests

Verify that:

- unsupported raw text is not injected into instructional prompt positions;
- educational routes do not receive deterministic model quantities unnecessarily;
- deterministic boundary conclusions cannot be contradicted by the LLM prompt;
- existing residual and prediction answers remain solely deterministic.

### Application tests

Test visible route responses and stale-state invalidation when the question, response, formula, data, or model changes.

## Compatibility requirements

Stage 47 must preserve:

- all existing supported follow-up categories;
- deterministic prediction, residual, comparable-observation, unit-change, and adjustment-comparison results;
- current extrapolation blocking;
- prompt-injection protection;
- the programmatic `runModel()` interface;
- existing explanation behaviour for ordinary answerable research questions;
- offline operation and mock-provider testing.

## Non-goals

Stage 47 is not intended to:

- build a general-purpose chatbot;
- let the LLM choose or fit arbitrary models;
- make causal conclusions from ordinary regression models;
- invent pass marks, clinical thresholds, affordability criteria, or definitions of success;
- infer missing personal predictor values;
- add new residual, diagnostic, prediction, or code-generation computations already delivered elsewhere;
- replace existing deterministic statistical engines with LLM calculations;
- solve every possible natural-language ambiguity in one stage.

## Proposed implementation sequence

### Stage 47.2: executable characterisation and route contract

- Add the unusual-question corpus as offline tests.
- Introduce the shared route object and validator.
- Wrap existing follow-up categories without changing visible behaviour.
- Route prediction-shaped research questions through the shared contract.

### Stage 47.3: unclear, purpose, and educational questions

- Detect empty-content, non-question, vague, purpose, and concept-definition requests.
- Add dedicated deterministic or constrained educational responses.
- Prevent conventional model interpretation from masquerading as the answer.

### Stage 47.4: missing information and definitions

- Generalise existing prediction `needs_input` handling.
- Detect missing predictor values and user-defined thresholds.
- Provide precise clarification requests.

### Stage 47.5: alternative-analysis guidance

- Detect response/question mismatch, pass/fail probability questions, conditional-distribution requests, causal questions, and diagnostic/model-adequacy requests.
- Explain the required alternative without automatically refitting the model.

### Stage 47.6: model-specific capability guidance

- Generate a small set of answerable example questions from the fitted model and available pathways.
- Use this for unclear or unsupported questions.

### Stage 47.7: integration and robustness review

- Review research-question and follow-up UX together.
- Test provider independence and prompt safety.
- Confirm no regressions in existing deterministic pathways.
- Consolidate documentation and developer diagnostics.

## Decision for Stage 47.2

The next stage should not begin by adding many new keyword categories directly to `classifyModelFollowupQuestion()`.

It should first introduce the shared route contract and an executable characterisation corpus. The existing classifier should be delegated to for mature bounded model questions. New high-level routes should then be added around it in small, separately tested stages.
