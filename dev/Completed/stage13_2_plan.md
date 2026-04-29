# WMFM Stage 13.2 implementation plan

## Goal

Turn the Stage 13.1 sweep findings into a small, testable hardening step. The goal is to improve prompt guidance and protect existing structural answer-selection behaviour without redesigning the explanation system.

## Scope

Stage 13.2 should do three things:

1. Add targeted prompt guidance for recurring wording and statistical-communication issues.
2. Add deterministic tests that check the prompt now contains the new guidance.
3. Add a structural answer-selection regression test for numeric-by-factor interaction explanations.

## In scope

### Prompt guidance

Add prompt text that tells the language model to:

- allow plain "the model" wording, but avoid qualified labels such as "fitted model", "linear model", "logistic model", or "Poisson model";
- preserve the response terminology from the research question and data documentation, especially "mark" rather than "score";
- avoid confidence-interval overlap or non-overlap as the justification for group differences;
- use direct weak-evidence wording such as "there is no clear evidence of a difference based on these data";
- state the numeric reference value when reporting group fitted values from models that also contain numeric predictors;
- prefer explicit numeric-effect wording such as "for each one-unit increase" over "per unit increase";
- include quantitative estimates for interaction comparisons where available.

### Tests

Add tests that verify:

- the generated prompt includes the new Stage 13.2 wording controls;
- interaction prompt skeletons ask for numeric support in within-group effect comparisons;
- structural answer selection keeps `answer` off intermediate slope statements and applies it to the final integrated conclusion.

## Out of scope

Do not do the following in Stage 13.2:

- add the discarded body.df Poisson examples;
- search for or introduce a new public Poisson two-factor dataset;
- redesign the sentence-role ontology;
- rename the legacy `answer` role to `finalAnswer`;
- rewrite the explanation prompt from scratch;
- require real LLM calls in tests;
- add development-only scripts as package test fixtures.

## Deliverables

- `stage13_2_changes.zip`
- `run_stage13_2.sh`
- updated prompt/rule files only where necessary
- focused deterministic tests

## Validation path

Run the Stage 13.2 script with:

```bash
bash run_stage13_2.sh --install-files --changes-zip /path/to/stage13_2_changes.zip
```

The script should:

1. install the change set if requested;
2. run `devtools::document()`;
3. run strict tests;
4. run strict check;
5. bump the package version only after checks pass;
6. commit the changes;
7. archive, build, install, and make the ChatGPT bundle.

## Recommended next step after Stage 13.2

After Stage 13.2 is installed, rerun the examples that produced Stage 13.1 feedback. If the same issue patterns remain, Stage 13.3 should move from prompt-guidance checks to prompt-payload or sentence-tagging implementation changes based on the new evidence.
