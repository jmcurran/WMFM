# WMFM Stage 8 Evaluation Report

## Overview

Stage 8 evaluated explanation quality across 19 developer-feedback JSON reports exported from the WMFM app.

The goal was not to fix issues immediately. The goal was to identify systematic failure modes, classify issues, infer likely root causes, and prepare the ground for a later implementation stage.

The evaluated models covered:

- Gaussian linear models
- Logistic binomial GLMs
- Poisson GLMs
- Intercept-only models
- Numeric predictors
- Factor predictors
- Additive models
- Interaction models

The central conclusion is:

The system has strong statistical capability but weak control systems.

In several examples, especially later Poisson models, the explanations were clear, statistically sound, and well structured. However, the system is inconsistent. The most important weaknesses are not usually basic statistical reasoning, but rather tagging, schema coverage, scale control, narrative structure, formatting, and plain-language translation.

## Stage-by-stage summary

### Stage 8.01 - LM intercept-only: Exam ~ 1

Main issues:

- The explanation used variation-explained language for an intercept-only mean model.
- The final summary sentence answered the research question but was not tagged as an answer.

Diagnosis:

- Model-awareness gap for intercept-only models.
- Early evidence of answer-tagging failure.

### Stage 8.02 - LM numeric predictor: Exam ~ Test

Main issues:

- Final sentence combined model fit, answer, and disclaimer.
- The sentence answered the research question but was not tagged as an answer.

Diagnosis:

- Sentence role overloading.
- Tagging system failed when the answer was embedded inside a multi-role sentence.

### Stage 8.03 - LM factor predictor: Exam ~ Attend

Main issues:

- Generally good explanation.
- Minor terminology issue: "regression results" should be replaced with app-consistent language such as "model results" or "the fitted information".

Diagnosis:

- Good example of clean role separation.
- Answer tagging worked when the answer was in a clean dedicated sentence.

### Stage 8.04 - LM additive factors: Exam ~ Attend + Gender

Main issues:

- A group comparison sentence was not tagged as comparison.
- A sentence using a confidence interval missed an uncertainty tag.
- Raised the question of whether confidence interval language counts as technical language.

Diagnosis:

- Weak detection of implicit comparisons.
- Incomplete uncertainty detection.
- Need clearer tag definitions.

### Stage 8.05 - LM factor interaction: Exam ~ Attend + Gender + Attend:Gender

Main issues:

- Research question restatement was misclassified as comparison.
- Baseline/reference case was misclassified as comparison.
- Narrative jump from one subgroup to a general attendance effect was incoherent.
- The explanation used "interaction term", which is too technical for students.

Diagnosis:

- Reference-category confusion.
- Technical language leakage.
- Interaction explanations degrade as model complexity increases.

### Stage 8.06 - LM numeric + factor additive: Exam ~ Attend + Test

Main issues:

- Research-question translation was technically accurate but pedagogically poor.
- Effect and confidence interval statements were poorly ordered.
- A confidence interval sentence missed uncertainty tagging.
- Average test score was displayed with too much precision.

Diagnosis:

- Poor translation of model constraints.
- Estimate-CI pairing needs stronger control.
- Deterministic numeric formatting gap.

### Stage 8.07 - LM numeric x factor interaction: Exam ~ Attend + Test + Attend:Test

Main issues:

- Research-question translation was worse than the original.
- "Fitted linear model" language leaked into the explanation.
- Slope/effect statements were misclassified as typical cases.
- Average test score was over-precise.
- Interaction explanation was confused.
- Final answer and justification were not properly tagged.

Diagnosis:

- Failure to distinguish level/value from rate of change.
- Interaction explanation breakdown.
- Missing schema role for justification/evidence.

### Stage 8.08 - Logistic intercept-only: Pass ~ 1

Main issues:

- Explanation talked about intercepts and log-odds.
- Claimed the baseline was the only predictor, although there were no predictors.
- Odds were not represented clearly.
- Answer sentence was tagged as uncertainty rather than answer.

Diagnosis:

- Major GLM scale leakage.
- Intercept-only model-awareness failure.
- No clear odds/probability communication policy.

### Stage 8.09 - Logistic numeric predictor: Pass ~ Assign

Main issues:

- Baseline at assignment score 0 was technically valid but pedagogically poor.
- Tiny odds were displayed as 0.001 rather than a readable odds form such as 1:1000.
- Explanation used odds despite the research question being about probability.
- Raw coefficient/exponentiation explanation appeared in student-facing text.
- Final answer was not tagged as answer.

Diagnosis:

- Pedagogically poor anchor selection.
- Odds/probability scale mismatch.
- Parameter/derivation leakage.

### Stage 8.10 - Logistic factor predictor: Pass ~ Attend

Main issues:

- "Fitted model" and "baseline odds" language was poor.
- Odds were shown as decimals rather than odds ratios or odds notation.
- "Seven times higher odds" was not clearly linked to the earlier group odds.

Diagnosis:

- Scale representation problem.
- Weak relational transparency: quantities are not clearly linked.

### Stage 8.11 - Logistic additive factors: Pass ~ Attend + Gender

Main issues:

- Odds had unnecessary precision and were not written as odds.
- Conditioning context was lost between sentences.
- One male subgroup sentence appeared numerically inconsistent: the numbers showed lower odds with attendance but the text said attendance raised the odds.
- Some answer-like content was not tagged as answer.

Diagnosis:

- Conditioning context loss.
- Numerical-verbal inconsistency, a critical trust issue.
- Persistent scale formatting problems.

### Stage 8.12 - Logistic factor interaction: Pass ~ Attend + Gender + Attend:Gender

Main issues:

- Key attendance multiplier was hard to trace back to model output.
- Technical language appeared: "fitted logit model" and "interaction".
- Partial answer was tagged as answer.
- Gender effect and interaction multiplier were unclear.
- Final answer was tagged as researchQuestion rather than answer.
- Disclaimer sentence was not recognised.

Diagnosis:

- Traceability failure.
- Partial vs full answer confusion.
- Interaction explanation breakdown in GLMs.
- Missing disclaimer category.

### Stage 8.13 - Logistic numeric + factor additive: Pass ~ Attend + Assign

Main issues:

- Explanation included low-value dataset metadata and coding details.
- Supporting information was misclassified as comparison.
- Assignment score 0 anchor was again poor.
- Odds representation was poor.
- Several effect sentences also expressed uncertainty but were not tagged as uncertainty.
- Mathematical expression 1.8^10 was confusing.
- "Evidence weaker" comparison was unclear.

Diagnosis:

- Signal-to-noise problem.
- Mathematical expression leakage.
- Anchor and scale issues repeated.

### Stage 8.14 - Logistic numeric x factor interaction: Pass ~ Attend + Assign + Attend:Assign

Main issues:

- Overall statistical story was relatively good.
- Assignment score 0 anchor remained poor.
- Odds were not represented as odds.
- CI derivation explanation was unnecessary.
- Several answer sentences were misclassified.
- Multiple answer-like sentences were redundant.

Diagnosis:

- Good interaction explanation is possible.
- Presentation, tagging, and formatting still fail.
- Redundant answer generation appears.

### Stage 8.15 - Poisson intercept-only: Freq ~ 1

Main issues:

- Explanation exposed log expected count and log intercept.
- Mentioned baseline level of predictors despite no predictors.
- Explained transformation back from log scale unnecessarily.
- Answer sentence was tagged as uncertainty.
- "Earthquakes per observation" was unclear.
- Final disclaimer was low value.

Diagnosis:

- GLM scale leakage extends beyond logistic models.
- Model-awareness failure in Poisson intercept-only model.
- Poor natural-language mapping from data structure to domain meaning.

### Stage 8.16 - Poisson numeric predictor: Freq ~ Magnitude

Main issues:

- On the whole, this was a very clear and good explanation.
- Final answer sentence was not tagged as answer.

What worked well:

- Correct response scale.
- Good anchor at average magnitude 6.25.
- Clear multiplier and percentage-decrease interpretation.
- Good pairing of effect and uncertainty.
- No technical leakage.
- Clean narrative flow.

Diagnosis:

- The system can produce excellent explanations.
- Answer tagging still failed even in a clean case.

### Stage 8.17 - Poisson factor predictor: Freq ~ Locn

Main issues:

- Generally good explanation.
- Minor phrase: "average expected frequency" was redundant.
- Typical-value sentence was tagged only as uncertainty.
- Group comparison tagging was incomplete.
- "Fitted model" appeared.
- Final sentence combined another answer and disclaimer but was not tagged.

Diagnosis:

- Strong explanation, weak tagging.
- Persistent technical-language and redundancy issues.

### Stage 8.18 - Poisson numeric + factor additive: Freq ~ Locn + Magnitude

Main issues:

- Good explanation overall.
- First sentence was a research question but tagged as effect/comparison.
- Minor "average expected" phrasing issue.
- Group comparison sentence was not tagged as comparison.
- Model-constraint sentence lacked a meaningful tag.
- Disclaimer sentence was not recognised.

Diagnosis:

- Schema gaps are now clear: modelConstraint and statisticalDisclaimer are needed.
- Tagging remains the most unstable component.

### Stage 8.19 - Poisson numeric x factor interaction: Freq ~ Locn + Magnitude + Locn:Magnitude

Main issues:

- High quality answer overall.
- A conclusion appeared too early, before supporting evidence.
- Multi-role typical-value and uncertainty sentence was incompletely tagged.
- A sentence containing effect, uncertainty, and comparison was tagged only as comparison.

Diagnosis:

- Narrative sequencing needs control.
- Multi-role sentence tagging remains weak.
- The system can explain complex interactions well, but inconsistently.

## Cross-example failure taxonomy

### 1. Tagging system instability

This was the most pervasive failure mode.

Observed failures:

- research-question statements missed or misclassified
- answer statements missed or misclassified
- partial answers treated as full answers
- answer statements tagged as researchQuestion
- uncertainty missed when sentences also contained effect or comparison
- comparison missed when expressed implicitly or via group values
- typical-case statements missed when paired with uncertainty
- supporting information misclassified as comparison
- disclaimers not classified
- model constraints not classified
- justification/evidence not classified

Likely root causes:

- surface-form-dependent tagging
- insufficient schema
- poor handling of multi-role sentences
- lack of explicit answer/justification/disclaimer detection

### 2. Incomplete tagging schema

The current schema seems too narrow.

Likely missing or underdeveloped roles:

- answer
- partialAnswer
- justification or evidence
- statisticalDisclaimer
- modelDescription or context
- modelConstraint
- scaleTranslation, if retained
- derivationDetail, if retained only for developer mode

### 3. GLM scale handling failures

Repeated issues:

- log-odds shown to students
- log expected counts shown to students
- intercepts shown to students
- raw coefficients shown to students
- explanation of exponentiation or back-transformation included unnecessarily
- odds and probability mixed without policy
- odds not formatted as odds

Likely root cause:

- explanation layer is receiving or using model-scale information without enough deterministic response-scale guardrails.

### 4. Anchor selection problems

Repeated issue:

- numeric predictor anchor selected as 0 because it is in range, even when it is pedagogically poor.

Better behaviour observed:

- magnitude examples used mean/median value 6.25 and were much clearer.

Likely root cause:

- anchor rules are mathematically valid but not always pedagogically suitable.

### 5. Model-awareness gaps

Repeated issues:

- intercept-only models described using predictor language
- baseline/reference language used when no predictors exist
- interaction explanations sometimes describe terms rather than comparisons
- R-squared used in an intercept-only LM context

Likely root cause:

- explanation templates or prompts do not branch strongly enough by model structure.

### 6. Technical language leakage

Recurring terms to avoid in student-facing explanations:

- fitted model
- fitted linear model
- fitted logit model
- fitted Poisson model
- regression results
- interaction term
- intercept
- log-odds
- log count
- exponentiating the coefficient
- baseline level of predictors

Likely root cause:

- LLM defaults and/or prompt language not sufficiently constrained.

### 7. Narrative structure problems

Observed issues:

- conclusions placed before evidence
- point estimates separated from their CIs
- answer sentences repeated
- sentences carrying too many roles
- final answers mixed with statistical disclaimers
- subgroup conditioning lost across sentences

Likely root cause:

- no strong narrative template or post-generation validation for sentence ordering and role structure.

### 8. Interaction explanation instability

Observed range:

- some interaction explanations were excellent
- others were confused, technical, or untraceable

Key issues:

- mentioning interaction terms
- focusing on components instead of differences in relationships
- unclear CI logic around difference in slopes or multiplicative changes
- partial answers to interaction questions

Likely root cause:

- interaction explanation needs model-type-specific scaffolding and precomputed interpretable quantities.

### 9. Traceability gaps

Observed issue:

- some quantities looked plausible but were difficult to connect to model output or audit information.

Likely root cause:

- support mapping and explanation text are not sufficiently tied to deterministic audit values.

### 10. Numerical-verbal inconsistency

Observed issue:

- one logistic additive-factor example had numbers indicating a decrease but text saying attendance raised the odds.

Likely root cause:

- LLM-generated interpretation not checked against deterministic sign/direction information.

### 11. Numeric formatting issues

Observed issues:

- unnecessary precision
- inconsistent rounding
- raw means such as 11.5671
- odds displayed as decimals
- very small odds not rescaled

Likely root cause:

- formatting rules not centralised or not consistently applied before explanation generation.

### 12. Signal-to-noise problems

Observed issues:

- dataset metadata included in main explanation
- coding details included without helping answer the research question
- vague disclaimers and low-value final sentences

Likely root cause:

- prompt does not sufficiently prioritise answering the research question over explaining the dataset/model mechanics.

## Severity ranking

### Tier 1: Critical systemic issues

1. Tagging system instability
2. Incomplete tagging schema
3. GLM scale-handling inconsistency
4. Model-awareness gaps
5. Anchor selection problems
6. Numerical-verbal inconsistency guards

### Tier 2: High-impact explanation issues

7. Interaction explanation instability
8. Narrative structure and sequencing
9. Traceability to audit/model outputs
10. Technical language leakage
11. Scale/odds/probability representation policy

### Tier 3: Presentation and polish

12. Numeric formatting
13. Redundancy and low-value sentences
14. Natural-language phrasing
15. Signal-to-noise reduction

## Positive findings

The system produced several strong explanations, especially in Poisson examples.

Strong examples showed:

- response-scale explanation
- meaningful anchors
- clear effect interpretation
- confidence intervals paired with estimates
- no raw model-scale leakage
- concise final answer
- coherent narrative flow

This means the system is not fundamentally incapable of good explanation. The problem is consistency and control.

## Recommended direction for Stage 9

Stage 9 should convert these findings into a fix strategy. The work should be triaged before code changes.

Recommended first steps:

1. Consolidate the failure taxonomy.
2. Redesign or extend the tagging schema.
3. Define deterministic numeric and scale formatting rules.
4. Define model-aware explanation policies by model family and predictor structure.
5. Define anchor-selection policy for student-facing interpretation.
6. Decide which fixes are deterministic, which are prompt-based, and which require schema/UI changes.
7. Implement in small stages with offline tests.

Recommended priority:

1. Tagging/schema fixes
2. Deterministic numeric formatting and scale display
3. GLM scale-leakage prevention
4. Anchor policy refinement
5. Interaction explanation guardrails
6. Narrative sequencing and redundancy control
7. Terminology cleanup and polish

## Final conclusion

Stage 8 successfully mapped the explanation system's behaviour across 19 examples.

The strongest overall conclusion is:

The WMFM explanation system can produce excellent explanations, but lacks robust control mechanisms to ensure that quality consistently.

The next implementation phase should focus on control, schema, deterministic formatting, and model-aware explanation scaffolding rather than simply asking the LLM to "do better".
