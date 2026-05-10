# WMFM Future Workstream Context - Confounder / Adjustment Variable Handling

## Background

A new WMFM app workstream is proposed around allowing users to identify some predictors as adjustment variables rather than interpretive variables.

The motivating idea is that users may include variables in a fitted model because they should be adjusted for, but they do not want the app to explain or interpret those variables as substantive effects.

In statistical terms, these variables may often be thought of as:

```text
confounding variables
```

or more generally:

```text
adjustment variables
```

The app should still include them in the fitted model, but the explanation should treat them differently from variables of primary interest.

---

# Core user story

A user fits a model in WMFM and selects predictors from the available variable buckets.

For each predictor in the factor and continuous/numeric buckets, the user should be able to mark that variable as an adjustment variable.

If marked as an adjustment variable:

- the variable is included in the model
- the model is adjusted for that variable
- the app does not provide a substantive interpretation of that variable's estimated effect
- the explanation may mention it only in a phrase such as:

```text
after adjusting for age and sex
```

or:

```text
holding the adjustment variables constant
```

The main explanatory focus should remain on the non-adjustment predictors.

---

# Terminology

The current app appears to use a bucket labelled:

```text
continuous
```

This may be worth renaming to:

```text
numeric
```

because not all numeric predictors are continuous in a strict statistical sense.

Proposed buckets:

- factor variables
- numeric variables

Potential terminology for checked variables:

- adjustment variable
- covariate
- control variable
- confounder

Recommended app-facing label:

```text
Adjust for this variable
```

Recommended internal term:

```text
adjustment variable
```

Avoid using only the word "confounder" internally or in the UI, because a variable may be adjusted for without being a true confounder in a causal sense.

---

# Proposed UI design

Each variable listed in the factor and numeric predictor buckets could have a checkbox.

Example UI concept:

```text
Factor variables
[ ] treatment group
[x] sex
[ ] region

Numeric variables
[ ] dose
[x] age
[x] baseline score
```

Checked variables are included in the model but treated as adjustment-only variables.

Unchecked variables are included and interpreted normally.

Potential checkbox label:

```text
Adjust only
```

or:

```text
Use for adjustment only
```

A tooltip or help text could say:

```text
Adjustment variables are included in the fitted model but are not interpreted as effects of primary interest.
```

---

# Model-fitting behavior

The adjustment flag should not remove the variable from the model.

The model formula should still include both:

- predictors of interest
- adjustment variables

Example:

```text
outcome ~ treatment + dose + age + sex + baselineScore
```

where:

- treatment and dose are variables of interest
- age, sex, and baselineScore are adjustment variables

The fitted model should remain statistically identical to the model that would be fitted if those variables were included normally.

The adjustment flag affects interpretation and display, not the fitted model itself.

---

# Explanation behavior

The natural-language explanation should not interpret adjustment variables as substantive findings.

For adjustment variables, avoid language such as:

```text
Age was associated with...
Sex had a large effect...
Baseline score increased the expected outcome by...
```

Instead, use compact adjustment language such as:

```text
After adjusting for age, sex, and baseline score, treatment group was associated with...
```

or:

```text
The model includes age, sex, and baseline score as adjustment variables, so the reported treatment effect is adjusted for these variables.
```

The explanation should focus on variables of interest.

---

# Fitted-equation behavior: open design question

The effect of adjustment variables on fitted-equation display is less obvious and should be treated as an explicit design decision.

Possible approaches:

## Option 1: Show full equation but visually de-emphasize adjustment variables

The equation includes all fitted terms, but adjustment variables are visually marked or grouped.

Pros:

- statistically transparent
- preserves exact fitted model
- avoids hiding model terms

Cons:

- may still invite interpretation of adjustment coefficients
- equation may remain cluttered

Possible display:

```text
Estimated outcome = intercept + treatment effect + dose effect + adjustment terms
```

with adjustment terms collapsed or visually labelled.

---

## Option 2: Show full equation in technical details only

The main explanation shows only interpretive variables.

The full fitted equation remains available in an expandable section such as:

```text
Full fitted equation
```

or:

```text
Technical model details
```

Pros:

- keeps main explanation aligned with user intent
- still allows transparency

Cons:

- requires UI change
- users may miss the adjustment terms unless they expand the details

---

## Option 3: Show an adjusted-effect equation

The app could present a simplified explanatory equation focused on variables of interest, with text indicating that adjustment variables are held constant.

Example:

```text
Expected outcome changes with treatment and dose, after adjusting for age, sex, and baseline score.
```

Pros:

- clearer for interpretation
- avoids over-emphasizing adjustment coefficients

Cons:

- not the literal fitted equation
- must be carefully labelled to avoid misleading users

---

## Initial recommendation

Use a hybrid approach:

1. Keep the full fitted equation available somewhere.
2. In the main explanation, emphasize only variables of interest.
3. Include one clear adjustment phrase naming the adjustment variables.
4. If the fitted equation is displayed prominently, label or group adjustment terms so they are not interpreted as primary findings.

---

# Prompting / LLM behavior

If the app sends model summaries to an LLM, the prompt should explicitly distinguish:

- variables of interest
- adjustment variables

The LLM should be instructed:

```text
Do not interpret adjustment variables as substantive findings. Mention them only as variables adjusted for, unless needed to explain the model structure.
```

The prompt should also instruct the model not to infer causality from adjustment.

Example prompt concept:

```text
The following predictors are adjustment variables: age, sex, baselineScore.
They are included in the model to adjust the estimates for the variables of interest.
Do not interpret their coefficients as substantive findings. Mention them only in an "after adjusting for..." phrase.
```

---

# Deterministic app behavior

This should not rely only on LLM compliance.

The app should pass explicit metadata through the model/explanation pipeline, such as:

```r
adjustmentVars = c("age", "sex", "baselineScore")
primaryVars = c("treatment", "dose")
```

That metadata should be available to:

- formula construction
- equation display
- model summaries
- LLM prompt construction
- deterministic fallback explanations
- scoring/grading tests if applicable

---

# Data model considerations

Variable-level metadata may need to include:

- variable name
- variable type
- selected/included status
- adjustment-only status
- display label
- modelling role

Possible internal structure:

```r
list(
    variable = "age",
    variableType = "numeric",
    included = TRUE,
    adjustmentOnly = TRUE,
    displayLabel = "Age"
)
```

This may be preferable to tracking only separate character vectors, because the UI may eventually need more variable-level settings.

---

# Testing considerations

Tests should verify that:

- checked adjustment variables are still included in the model formula
- adjustment variables are passed into explanation metadata
- adjustment variables are not interpreted as substantive effects
- explanation includes an "after adjusting for..." style phrase
- non-adjustment variables continue to be interpreted normally
- deterministic fallback behavior handles adjustment variables
- tests run offline and do not require real LLM calls

Potential test examples:

```text
age is checked as adjustment-only
treatment is not checked
```

Expected explanation behavior:

```text
After adjusting for age, treatment group...
```

Not expected:

```text
Age was associated with...
```

---

# Important design risks

## 1. Confounder wording may imply causality

The app should avoid implying that checked variables are true causal confounders unless the user explicitly says so.

Prefer:

```text
adjustment variable
```

over:

```text
confounder
```

in most UI contexts.

---

## 2. Hiding adjustment terms may reduce transparency

The app should not make it look as though adjustment variables were omitted from the model.

If the main explanation suppresses their interpretation, the technical details should still make clear that they were included.

---

## 3. Equations need careful handling

A fitted equation is both:

- a mathematical representation of the full model
- an interpretive teaching object

Adjustment variables create tension between those two roles.

This should be designed intentionally rather than handled as an incidental prompt change.

---

## 4. Interactions involving adjustment variables

Open question:

What happens if a variable marked as adjustment-only participates in an interaction?

Examples:

```text
treatment * age
treatment * sex
```

Possible rules:

- disallow adjustment-only variables in interactions
- allow them but suppress direct interpretation
- interpret only the portion involving the primary variable
- require a warning or special explanation

This likely needs a separate design decision.

---

# Suggested future stages

## Stage 20.x or separate workstream

This workstream could be broken into stages such as:

### Stage A: Design audit

- identify where variable roles are currently stored
- inspect factor/numeric bucket UI code
- inspect formula construction
- inspect explanation prompt construction
- inspect equation display path

### Stage B: UI metadata implementation

- add adjustment-only checkboxes
- persist selected adjustment variables
- pass adjustment metadata through reactive state

### Stage C: Formula and equation behavior

- ensure adjustment variables remain in the fitted model
- decide how equations display adjustment terms
- add technical-detail grouping if needed

### Stage D: Explanation behavior

- update deterministic explanation logic
- update LLM prompts
- add explicit "after adjusting for..." wording
- suppress substantive interpretation of adjustment variables

### Stage E: Tests and regression protection

- add offline tests for adjustment-variable behavior
- add fake-provider tests for LLM prompts
- add UI/server tests where practical

---

# Suggested next-chat opener

```text
I want to begin a new WMFM workstream on adjustment variables. The idea is that variables in the factor and numeric predictor buckets can be checked as adjustment-only variables. They should still be included in the fitted model, but the explanation should not interpret their effects except to say something like "after adjusting for...". Here is the context document.
```
