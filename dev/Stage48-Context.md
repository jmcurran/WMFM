# WMFM Stage 48 context

We are working on the WMFM R package.

Stage 47, intelligent question handling, is complete.

The authoritative completed archive is:

```text
stage47.9.2.1_completed.zip
```

The current branch is:

```text
stage-47-intelligent-question-handling
```

## Stage 47 outcome

Stage 47 introduced intelligent handling for questions that should not simply trigger an ordinary interpretation of the fitted model.

The completed system now recognises and responds appropriately to questions involving:

- unclear or incomplete research questions;
- uncertainty about what to ask;
- questions about the purpose or usefulness of the analysis;
- questions about whether the current model can answer something;
- requests for alternative questions or models;
- undefined quantities, such as asking for a chance of passing without defining “pass”;
- unsupported causal claims;
- questions about model quality that require diagnostics;
- requests to explain the meaning of a result.

For these routes, specialised deterministic guidance replaces the ordinary coefficient-by-coefficient model explanation.

The focused evaluation suite is:

```text
intelligent_question_handling
```

It contains twelve questions:

1. Why are you predicting my exam result?
2. Why are we doing this?
3. I don’t know.
4. What is the chance I will pass the course?
5. Can this model answer my question?
6. What should I ask instead?
7. Is this analysis useful?
8. What does this result mean?
9. Does this prove attendance improves marks?
10. I don’t understand the question.
11. Tell me whether the model is any good.
12. Should I use a different model?

The final evaluation confirmed that:

- all twelve questions are routed correctly;
- specialised guidance replaces the ordinary model summary;
- unsupported predictions and causal claims are not invented;
- the responses are deterministic and therefore complete quickly;
- the stage runner installs the updated package and removes the superseded archive.

Stage 47 should now be regarded as complete.

---

# Stage 48: Consolidation, concise explanations, and reproducible analysis code

There are three remaining areas worth considering.

## 1. Small response-presentation polish

Some deterministic guidance contains numbered suggestions rendered inline, for example:

```text
1. ... 2. ... 3. ... 4. ...
```

These should instead be displayed as properly separated lines or bullets.

This is cosmetic and should not require changes to question routing.

## 2. Explanation length and concision

WMFM was originally intended to provide relatively short, student-friendly explanations.

Over time, some responses have become too verbose. They may include:

- a long description of the whole model when the student asked a narrow question;
- repeated numerical conclusions;
- unnecessary explanation of every coefficient;
- background material that is correct but not needed for the immediate question;
- deterministic material followed by overlapping LLM commentary.

Stage 48 should review whether the current response system still honours the intended idea of a “short explanation”.

Possible objectives include:

- defining explicit expectations for short, standard and detailed explanations;
- making the default response genuinely concise;
- answering the student’s actual question before giving supporting detail;
- avoiding repetition between deterministic and LLM-generated material;
- limiting ordinary model summaries to the most important findings;
- retaining enough numerical evidence to remain statistically useful;
- allowing the user to request more detail conversationally;
- ensuring that concision does not produce overconfident or misleading statements.

This should be evaluated across linear, logistic and Poisson models, rather than only with the Course example.

## 3. Providing reproducible R code

WMFM should eventually be able to provide students with R code corresponding to the analysis they have performed.

This is more complex than simply reproducing the model call.

The generated code may need to represent:

- data loading or dataset selection;
- response and explanatory-variable selection;
- factors and reference levels;
- transformed variables;
- interactions;
- linear, logistic and Poisson model fitting;
- fitted equations;
- estimated means or probabilities;
- confidence intervals;
- contrasts;
- robust standard errors;
- predictions;
- diagnostic plots;
- any options selected in the application.

Particular care is needed for fitted equations and adjusted means. These may not correspond to a single simple line of R code and may require:

- extracting and formatting coefficients;
- constructing reference or representative predictor values;
- using `predict()` with appropriate scales and intervals;
- using estimated marginal means or equivalent calculations;
- explaining any averaging or conditioning choices;
- ensuring that displayed values can be reproduced from the emitted code.

The generated code should be:

- valid R code;
- reproducible outside the Shiny application;
- based on the actual fitted model rather than reconstructed from prose;
- reasonably readable by students;
- explicit about required packages;
- consistent with WMFM’s displayed results;
- robust to transformed terms, factors and interactions.

## Proposed Stage 48 approach

Stage 48 should begin with an audit and design phase rather than immediately implementing every form of code generation.

A sensible sequence is:

### Stage 48.1 — Response-length audit

- characterise current response length and repetition;
- identify where deterministic and LLM content overlap;
- define the intended behaviour of short, standard and detailed explanations;
- create representative evaluation examples across model families.

### Stage 48.2 — Concise default responses

- make the default explanation materially shorter;
- preserve numerical correctness and essential uncertainty;
- retain conversational requests for additional detail;
- repair the numbered-list presentation issue.

### Stage 48.3 — Analysis-code requirements and architecture

- identify the complete analysis state that must be serialised;
- distinguish model-fitting code from post-fit calculation code;
- define a renderer-independent internal representation of the performed analysis;
- decide which code-generation cases are initially supported;
- create exact reproducibility tests comparing generated-code results with WMFM results.

### Later Stage 48 substages

Possible implementation could then proceed incrementally:

1. model-fitting code;
2. fitted-value and prediction code;
3. fitted equations;
4. estimated means and probabilities;
5. contrasts and robust inference;
6. diagnostic and plotting code.

The code-generation work should not rely on asking the LLM to invent R code from a prose description. WMFM should deterministically construct the code from the same stored analysis state used to fit and display the model. The LLM may explain that code, but it should not be the source of truth.

---

# Immediate decision

No Stage 48 implementation has yet begun.

The next step should be to decide whether to:

1. pause development after recording this context; or
2. begin Stage 48.1 as a limited audit of explanation length and response structure.

The larger analysis-code feature should remain explicitly scoped as design work until its reproducibility requirements have been established.
