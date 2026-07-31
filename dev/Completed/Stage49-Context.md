# WMFM Stage 49 context

We are working on the WMFM R package.

Stage 48 developed the student explanation workflow and should be merged before Stage 49 begins.

The authoritative completed Stage 48 archive is:

```text
stage48.10_completed.zip
```

The installed package version in that archive is:

```text
1.1.5.014
```

The Stage 48 merge script is:

```text
wmfm_merge_stage48.sh
```

It should be run from the completed Stage 48 branch. The script discovers the current source branch, merges it into `master`, and can optionally create the Stage 49 branch:

```bash
bash wmfm_merge_stage48.sh --create-stage49-branch
```

The proposed Stage 49 branch is:

```text
stage-49-question-aware-explanations
```

## Completed Stage 48 outcome

Stage 48 now provides an explicit student explanation workflow rather than treating the LLM explanation as the only account of a fitted model.

The completed work includes:

- an explicit request to explain a fitted model;
- a student explanation editor;
- formative grading and feedback;
- model-aware statistical insertion tools;
- a compact insertion toolbar for coefficients, expected responses, predictions, comparisons, residuals, and other statistics;
- model-specific support for linear, logistic, and Poisson regression;
- individual prediction insertion, including prediction intervals for linear models;
- direct entry of predictor profiles rather than selecting existing observations;
- fitted-mean and confidence-interval insertion from user-defined profiles;
- developer-only explanation-grading examples;
- compact developer diagnostics that omit the data and fitted-model internals;
- prediction-aware semantic grading;
- removal of ordinary causal or overclaim wording from the fatal-flaw 40% cap; and
- student-facing feedback organised as `What you did well` and `What you need to revise/improve`.

Stage 48 also demonstrated an important distinction between:

- explaining a fitted model; and
- answering the research question that motivated the model.

That distinction is the starting point for Stage 49.

---

# Stage 49: Question-aware and prediction-first explanations

## Motivation

A developer-only Course example uses the model:

```r
Exam ~ Attend + Test
```

and the research question:

```text
Will I do well on the final exam if I attend class regularly and get a good mark in the test?
```

This is primarily an individual-prediction question.

A suitable answer should begin with a specified prediction, for example for a student with:

```text
Attend = Yes
Test = 15
```

The fitted model gives approximately:

```text
Predicted exam mark: 67.5
95% prediction interval: 44.6 to 90.5
```

The answer should explain that the expected mark is reasonably high but that the interval for an individual student is wide, so the model does not guarantee that the student will do well.

The current LLM response instead concentrated on:

- the attendance coefficient;
- the test coefficient;
- mean responses for attendance groups;
- confidence intervals for average responses; and
- the proportion of variation explained.

Those statements were useful model interpretation, but the response missed the prediction request almost completely. It answered a different question: “What does the fitted regression model say?” rather than “What does the model predict for me?”

## Central Stage 49 objective

WMFM should identify the statistical purpose of the research question and organise the response around that purpose.

The research question must not merely be pasted into a general model-explanation prompt. WMFM should derive a structured question objective and provide it to both deterministic and LLM components.

The primary answer should come first. General model interpretation should be supporting material rather than the default organising structure.

## Proposed question archetypes

The first implementation should support a small, explicit set of archetypes:

1. **Explain the fitted model**
   - Summarise the principal fitted relationships.
   - Interpret the most important coefficients and uncertainty.

2. **Estimate or interpret an effect**
   - Focus on the named predictor or contrast.
   - State direction, magnitude, scale, uncertainty, and reference group where relevant.

3. **Compare groups or profiles**
   - Define both groups or profiles.
   - State the comparison on an appropriate response scale.
   - Include uncertainty where available.

4. **Predict an individual outcome**
   - Require or infer a complete predictor profile.
   - Lead with the individual prediction.
   - Include a prediction interval when the fitted model supports one.
   - Interpret practical uncertainty and avoid presenting a prediction as a guarantee.

5. **Estimate an expected or average response**
   - Lead with the fitted mean, probability, or expected count for a defined profile.
   - Include a confidence interval where supported.
   - Distinguish this from an individual prediction.

6. **Explain uncertainty**
   - Focus on the requested interval, standard error, or uncertainty statement.
   - Explain what the interval does and does not describe.

7. **Assess whether the model can answer the question**
   - Identify missing variables, undefined outcomes, unsupported causal language, extrapolation, or a mismatch between the response and the question.
   - Offer a more answerable question or a more suitable model where appropriate.

These archetypes should build on Stage 47 intelligent question handling rather than duplicate it.

## Structured question objective

A new deterministic object should represent the interpreted purpose of the question. A possible shape is:

```r
list(
  archetype = "individual_prediction",
  primaryObjective = paste(
    "Predict the final exam mark for a student who attends regularly",
    "and receives a specified test mark."
  ),
  profile = list(
    Attend = "Yes",
    Test = 15
  ),
  essentialConcepts = c(
    "individual prediction",
    "prediction interval",
    "practical uncertainty"
  ),
  supportingConcepts = c(
    "attendance effect",
    "test-mark effect",
    "model fit"
  ),
  unsupportedOrMissing = character()
)
```

The exact class and field names should be decided during the design substage. The important requirement is that the objective is deterministic, inspectable, and testable.

## Response ordering contract

For an individual-prediction question, the generated response should normally follow this order:

1. answer the prediction question directly;
2. provide the prediction and prediction interval;
3. interpret whether the predicted outcome meets the practical criterion in the question;
4. explain the uncertainty for an individual outcome;
5. give only the model interpretation needed to support the answer; and
6. optionally offer broader model detail after the primary answer.

For the Course example, an appropriate opening would be similar to:

> A student who attends class regularly and scores 15 out of 20 on the test is predicted to receive about 67.5 marks on the final exam. The 95% prediction interval is approximately 44.6 to 90.5, so the expected result is a pass, but the model cannot guarantee that this individual student will do well.

The exact wording should remain model-aware and student-friendly.

## Deterministic calculations remain the source of truth

The LLM must not invent the predictor profile, prediction, interval, reference group, or numerical interpretation.

WMFM should deterministically calculate and pass to the prompt:

- the question archetype;
- the primary objective;
- the supplied or resolved predictor profile;
- the prediction or expected response;
- the correct interval type;
- the practical criterion, when defined;
- relevant supporting effects; and
- any limitations or missing information.

The LLM may organise and explain these facts, but it should not be responsible for deriving them from prose.

## Missing information

A question such as “Will I do well?” is not fully specified unless WMFM knows:

- what predictor values represent the student; and
- what “do well” means.

Stage 49 should define when WMFM may use a supplied profile, when it may use a developer example profile, and when it must ask a follow-up question.

It should not silently substitute an “average” student for an individual-prediction question.

Possible deterministic follow-ups include:

```text
What test mark should I use for this student?
```

and:

```text
What final-exam mark would count as doing well?
```

If a threshold is not supplied, WMFM may describe the predicted mark and interval without claiming that the student will pass or do well.

## Model-family requirements

The first design and tests should cover all supported model families.

### Linear models

- individual fitted value;
- prediction interval;
- expected mean and confidence interval;
- distinction between individual and mean uncertainty.

### Logistic models

- predicted probability;
- odds or log odds only when requested or pedagogically useful;
- no unsupported individual prediction interval;
- threshold-based classification only when the threshold is explicitly defined.

### Poisson models

- expected count;
- response-scale interpretation;
- no claim that the expected count is the exact future count;
- uncertainty described using only intervals the deterministic calculation actually supplies.

## Prompt architecture

The prompt should have an explicit priority order, such as:

```text
Primary task:
Answer the student's research question directly.

Question archetype:
Individual prediction.

Required content:
- state the supplied profile;
- state the predicted outcome;
- state and interpret the prediction interval;
- explain that an individual outcome remains uncertain.

Supporting content:
- briefly explain the attendance and test-mark relationships if useful.

Do not replace the requested prediction with a general coefficient summary.
```

The ordinary model summary should not be allowed to displace the primary task.

## Evaluation and grading alignment

Stage 49 should evaluate whether generated explanations answer the correct question, not only whether their individual statistical statements are valid.

For a prediction question, a response should lose substantial credit if it omits the prediction even when its coefficient interpretations are correct.

Suggested deterministic evaluation fields include:

```text
questionArchetypeRecognised
primaryObjectiveAnswered
requiredProfileStated
individualPredictionStated
predictionIntervalStated
practicalUncertaintyExplained
meanAndPredictionDistinguished
supportingModelInterpretationProportionate
```

These should be visible in developer diagnostics.

## Developer examples

Retain the existing developer-only Course explanation-grading example and add a developer-only generated-explanation example that preloads:

```text
Model: Exam ~ Attend + Test
Research question: Will I do well on the final exam if I attend class regularly and get a good mark in the test?
Profile: Attend = Yes, Test = 15
```

This should allow repeated testing without reconstructing the model, question, and prediction profile each time.

The example should support comparison between:

- the current general model-summary response; and
- the new question-aware prediction-first response.

## Carried-forward feedback presentation issue

Stage 48.10 introduced student-friendly `What you did well` and `What you need to revise/improve` sections.

One remaining presentation issue should not be lost:

- `comparisonStructureClear`;
- `referenceGroupHandledCorrectly`; and
- `referenceGroupCoverageAdequate`

can all describe the same incomplete comparison and may currently produce three near-duplicate revision comments.

The student-facing layer should group related metrics into an issue family and show one actionable comment for the underlying problem. The complete separate metric deductions should remain available in developer diagnostics.

The two feedback sections may also use accessible Git-style visual treatment:

- pale green with a green border and positive icon for `What you did well`;
- pale red or pink with a red border and revision icon for `What you need to revise/improve`.

Colour must not be the only cue.

This is a small carried-forward refinement and should be completed either as Stage 49.1 or before the main question-archetype implementation begins.

## Proposed Stage 49 sequence

### Stage 49.1 — Feedback issue-family deduplication and accessible styling

- group related internal metrics into student-facing issue families;
- display at most one revision message per underlying issue;
- retain detailed metrics in developer diagnostics;
- add accessible green and red feedback panels;
- test the missing-`more` attendance sentence as a single comparison issue.

### Stage 49.2 — Question-archetype design and characterisation

- define the archetype object and routing rules;
- characterise current responses across linear, logistic, and Poisson examples;
- establish cases that require follow-up information;
- add developer diagnostics for the resolved objective.

### Stage 49.3 — Individual-prediction route

- implement deterministic prediction-objective resolution;
- make the answer prediction-first;
- supply deterministic profile, prediction, and interval facts to the LLM;
- add the developer-only Course example;
- prevent a general model summary from satisfying the route by itself.

### Stage 49.4 — Expected-response and comparison routes

- distinguish mean-response questions from individual predictions;
- support group and profile comparisons;
- ensure confidence intervals and prediction intervals are never interchanged.

### Stage 49.5 — Cross-model-family extension

- extend and test the architecture for logistic and Poisson models;
- ensure model-family limitations are stated accurately;
- add focused offline evaluation cases.

### Stage 49.6 — Evaluation and prompt robustness

- add question-answer alignment metrics;
- expand developer diagnostics;
- test concise, standard, and detailed explanation modes;
- ensure deterministic content is not contradicted or displaced by the LLM.

## Initial acceptance criteria

Stage 49 should not be considered complete until:

1. the Course research question is classified as an individual-prediction question;
2. the supplied profile is stated in the answer;
3. the answer leads with the prediction rather than the coefficients;
4. the prediction interval is included and correctly interpreted;
5. the response does not claim certainty about an individual outcome;
6. supporting coefficient interpretation remains concise and secondary;
7. missing profile values or practical thresholds trigger an appropriate follow-up rather than silent substitution;
8. developer diagnostics show the archetype, objective, required concepts, supplied facts, and omissions;
9. regression tests cover linear, logistic, and Poisson examples; and
10. the existing Stage 47 specialised question routes continue to work unchanged.

## Immediate next step

After merging Stage 48, create the Stage 49 branch and begin with Stage 49.1 or Stage 49.2.

The recommended order is Stage 49.1 first because it is small and already understood, followed by the question-archetype design audit in Stage 49.2.
