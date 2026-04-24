# WMFM future work context: LLM-assisted contrast specification

I am working on an R package/app called WMFM.

This note is for later work, not for the current workflow.

## Background

WMFM already has functions for computing contrasts and confidence intervals for contrasts.

I am interested in allowing a user to ask a natural-language question such as:

- Is the mean number of oysters at the George River different from the average of the two Port Stephens sites?

and then using the package's existing deterministic contrast machinery to answer it.

## Core idea

Do not let the LLM do the statistics directly.

Instead, use the LLM to translate a natural-language question into a structured contrast specification, and then let WMFM compute the contrast and confidence interval deterministically using existing package functions.

The intended workflow is:

1. User asks a natural-language question.
2. The LLM maps that question to a structured contrast specification.
3. R code validates the specification.
4. WMFM computes the contrast estimate and confidence interval using existing contrast functions.
5. The LLM explains the result in plain language.

## Design preference

I do not want the LLM to have unrestricted access to arbitrary R functions.

I do want a constrained interface where the LLM can choose from supported contrast patterns or fill in a structured contrast object that WMFM then validates and computes.

So the preferred design is:

- LLM specifies the contrast
- WMFM computes the contrast
- LLM explains the result

not:

- LLM performs unrestricted function calling into arbitrary package internals

## Example motivating question

For the oysters example, a natural question is:

- Is the mean number of oysters at the George River different from the average of the two Port Stephens sites?

Conceptually this corresponds to comparing:

mu_GR - 0.5 * (mu_PS1 + mu_PS2)

The important point is that WMFM should decide how that contrast is computed appropriately for the fitted model and interpretation scale, rather than letting the LLM improvise the algebra.

## Suggested architecture

A good first design would be for the LLM to return a structured object such as:

```json
{
  "contrastType": "group_average_difference",
  "targetGroup": "GR",
  "referenceGroups": ["PS1", "PS2"],
  "weights": [1, -0.5, -0.5]
}
```

WMFM would then:

- validate that the named levels exist
- validate the weights
- validate that the requested contrast is supported for the fitted model
- compute the estimate and confidence interval
- return a result object that can then be explained in plain language

## Why this seems attractive

This fits the existing WMFM philosophy well:

- deterministic statistical computation
- explicit validation
- student-friendly explanation
- offline testability
- less risk of the LLM inventing invalid statistical operations

It also fits naturally with the longer-term "Any other questions?" direction in the app.

## Initial scope suggestion

The safest first step would be to define a small contrast-spec interface rather than supporting arbitrary contrast algebra.

For example, supported forms might include:

- compare two levels
- compare one level against the average of several levels
- compare a custom weighted combination of levels
- possibly compare fitted values at specified predictor settings

## What I will likely want in the future

When I come back to this work, I will probably want help with:

1. designing a contrast-spec object or class
2. deciding which contrast types WMFM should support initially
3. identifying which existing package functions should sit behind that interface
4. designing validation rules
5. deciding whether the LLM should emit JSON, a named list, or another structured form
6. connecting the computed contrast result back into the explanation workflow

## Important implementation principle

The LLM should help map user intent to a structured request.

WMFM should remain responsible for the actual statistical computation.
