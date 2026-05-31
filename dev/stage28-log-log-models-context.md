# Stage 28 - Log-log models

## Branch

Suggested branch name after Stage 28.2:

```bash
log-log-models
```

The stage originally used `power-law-models`, but Stage 28.2 renames the development language to avoid misleading student-facing terminology.

## Goal

Add explicit support for log-log model workflows where students fit models of the form:

```r
log(y) ~ log(x)
```

The app should help students understand that this is a transformed linear model whose fitted relationship is best interpreted through proportional changes.

## Terminology decision from Stage 28.2

Use the following terminology split.

- Internal metadata: `logLog`
- Internal code, comments, and tests: "log-log model"
- UI model type display: "Log-log model"
- Student explanations: "proportional-change relationship"
- Follow-up questions: percentage changes or meaningful domain-specific changes
- Equation tab: show both the fitted log-log equation and the original-scale multiplicative equation

Avoid the student-facing terms "power law" and "elasticity" unless a user explicitly asks for those terms. The term "elasticity" is technically correct but is strongly associated with econometrics. The term "power law" is overloaded and can be misleading because it is often used for claims about heavy-tailed distributions or scale-free behaviour. A log-log regression may imply an original-scale multiplicative equation of the form `y = a * x^b`, but that does not mean the data-generating process should be described as a power-law process.

Preferred student-facing wording:

> The model was fitted after taking logs of both variables. This means the relationship is being interpreted in terms of proportional changes rather than ordinary one-unit changes.

Preferred example wording:

> A 10% increase in carat weight is associated with an estimated percentage change in price.

Avoid wording such as:

> The elasticity is ...

or:

> This is a power-law model.

## Motivation

Students often encounter relationships where proportional changes in `x` are more meaningful than one-unit additive changes. A model fitted as `log(y) ~ log(x)` should not be explained as an ordinary additive linear model on the original response scale.

The app should distinguish:

- the fitted model on the log-log scale
- the implied original-scale multiplicative relationship
- the interpretation of the slope as a proportional-change summary
- the limits of back-transforming fitted values and intervals

## Suggested scope

1. Detect simple log-log model forms.
2. Build deterministic metadata describing the transformation.
3. Add plain-language explanation support using proportional-change language.
4. Add equation-display support for the original-scale multiplicative expression.
5. Add deterministic tests using offline examples.
6. Preserve existing behaviour for ordinary transformed-response models.

## Teaching requirements

Explanations should make clear that:

- the fitted model is linear after taking logs
- the slope describes the approximate percentage change in `y` for a percentage change in `x`
- on the original scale the relationship is multiplicative, not additive
- fitted values may be back-transformed, but uncertainty needs careful wording

## Diamond examples

Use the `ggplot2::diamonds` data.

Suggested examples:

- Diamonds II: `log(price) ~ log(carat)`
- Diamonds III: `log(price) ~ log(carat)` with a follow-up asking for a more meaningful 0.1-carat change
- Diamonds IV: `log(price) ~ log(carat) + cut + color + clarity` with a follow-up asking whether adjusting for cut, color, and clarity substantially improves prediction

Research question:

> Can we predict the price of diamonds on the basis of weight?

Preferred interpretation sequence:

1. Primary interpretation: percentage change in price for a percentage change in carat weight.
2. Intermediate follow-up: change from 1.0 to 1.1 carats, or another domain-relevant 0.1-carat change.
3. Optional advanced follow-up: doubling carat weight.
4. Adjustment follow-up: whether cut, color, and clarity substantially improve predictive performance.

## Development conventions

Continue to use:

- `=` assignment
- camelCase identifiers
- braces on all control structures
- roxygen2 documentation
- modular helper functions
- deterministic offline tests
- thin orchestration layers

## Stage 28.3 implementation note

Stage 28.3 extends the log-log/proportional-change work to bounded unit-change follow-up questions. A request such as "Can you express the weight effect for a 0.1 carat increase?" cannot be answered from the slope alone, because a fixed original-scale increase corresponds to a different proportional change depending on the starting value. WMFM therefore uses a deterministic typical original-scale reference value recovered from the fitted log-predictor values, then converts the requested increase into a proportional predictor change before computing the fitted response multiplier and percentage change.

Student-facing wording should continue to avoid "elasticity" and "power law". The prompt payload should describe the model as log-log internally and should explain fixed original-scale changes using the chosen reference value and proportional-change language.


## Stage 28.5 note - proportional-change follow-ups

Stage 28.5 extends the log-log pathway so bounded follow-up questions can ask for
percentage changes directly. Student-facing explanations should prefer phrasing such as
"a 10% increase in carat" or "doubling carat weight" rather than introducing
econometrics-specific elasticity language or broad power-law terminology.

The deterministic payload should carry the predictor percentage change, the implied
predictor multiplier, the response multiplier, and the response percentage change.
This keeps log-log support aligned with the Stage 28 terminology decision: log-log
internally, proportional-change externally.

## Stage 28.7 note - deterministic adjustment assessment labels

Stage 28.7 adds a small deterministic classification layer to the Diamonds IV
adjustment-comparison follow-up. Stage 28.6 already exposed the adjusted and
weight-only log-log model summaries, but the follow-up asks whether adjustment
substantially improves prediction. To avoid leaving that judgement entirely to
the LLM, WMFM now adds a conservative in-sample assessment label based on
adjusted R-squared, residual standard error, and AIC changes.

The assessment is deliberately described as an in-sample fit summary. It must not
be presented as proof of better out-of-sample prediction, and it should continue
to use log-log internally and proportional-change language externally.

## Stage 28.8 note - wrap-up terminology and teaching checks

Stage 28.8 is the final Stage 28 polish pass. It keeps the development split
settled during Stage 28.2:

- use `log-log` for internal metadata, source files, tests, and developer notes
- use proportional-change wording in student-facing explanation guidance
- avoid student-facing `elasticity` language because it is strongly associated
  with econometrics
- avoid student-facing `power law` language because it is overloaded and can be
  confused with claims about heavy-tailed distributions or scale-free processes

The Diamonds II, III, and IV examples are intended to teach a sequence:

- Diamonds II: fit `log(price) ~ log(carat)` for the main research question
- Diamonds III: express the same relationship through a bounded 0.1-carat
  follow-up, using a typical reference value because a fixed original-scale
  change has different proportional meaning at different starting weights
- Diamonds IV: compare the adjusted log-log model with the simpler weight-only
  log-log model, describing the result as deterministic in-sample fit on the
  log-response scale rather than proof of better future prediction

This wrap-up adds regression tests for the terminology rule and the example
questions so later prompt or example edits do not accidentally reintroduce the
terms `elasticity` or `power law` into student-facing guidance.


## Stage 28.8.2 manual example-load fix

Manual testing of Diamonds II showed two app-level regressions that were not
caught by the earlier specification-only tests. The optional follow-up question
placeholder still used parser-friendly examples and mentioned a 10-unit increase,
which is not suitable for log-log teaching examples. The placeholder is now
phrased as natural language and avoids a default 10-unit change.

The same manual test also showed that loading a transformed example formula such
as `log(price) ~ log(carat)` could be overwritten by the automatic formula builder
after the example was loaded. The example loader now assigns buckets using the
original variables found inside transformed formula terms, so `carat` is assigned
as numeric, and it reapplies the exact example formula after the Shiny input
flush so the formula text remains `log(price) ~ log(carat)`.

The derived-variable path remains an intended compatibility requirement: if a
future example fits variables such as `logPrice ~ logCarat`, the equation display
should make clear that these are transformed versions of `price` and `carat`
when that metadata is available.


## Stage 28.8.3 example-loading repair

Manual testing showed that the Diamonds examples could still display an
auto-generated ordinary formula such as `price ~ carat` after loading, even
though the example specification stored `log(price) ~ log(carat)`. The repair
keeps transformed example formulas out of the ordinary auto-formula replacement
path so the explicit log-log formula remains visible in the model formula box.

Diamonds II has no follow-up question. Diamonds III carries the meaningful
0.1-carat follow-up. Diamonds IV carries the adjustment-comparison follow-up
and pre-selects `cut`, `color`, and `clarity` as adjustment variables.

The `ggplot2::diamonds` variables `cut`, `color`, and `clarity` are ordered
factors. For these teaching examples they are converted to ordinary factors
when the example data are loaded, so model summaries and explanation payloads
use treatment-style categorical terms rather than orthogonal polynomial
contrast terms.

## Stage 28.8.5 note

Stage 28.8.5 fixes a stale UI state issue in example loading. Examples that do not define a follow-up question, such as Diamonds II, must explicitly clear both the server-side `rv$modelFollowupQuestion` value and the visible `modelFollowupQuestion` text area. This is important when the user loads an example with a follow-up question and then loads a second example without one.

The named `dataTransform: diamondsPlainFactors` mechanism is a short-term compatibility path. A future transformation/provenance workstream should support explicit example transformations, for example:

```yaml
dataTransform:
  - cut = factor(as.character(cut))
  - color = factor(as.character(color))
  - clarity = factor(as.character(clarity))
```

That future design should also record how derived variables were created so that fitted equations and explanations can recover relationships such as `log.carat = log(carat)` and `log.price = log(price)`.

## Stage 28.8.6 prediction-parser regex fix

Manual testing of Diamonds II exposed a prediction-parser bug when fitted model
predictors contain transformation syntax. The natural-language prediction parser
was inserting predictor names such as `log(carat)` directly into a regular
expression, so the parentheses were interpreted as regex syntax and caused a
PCRE compilation error.

The fix treats predictor names as regex literals before building natural-language
numeric prediction patterns. This keeps transformed terms such as `log(carat)`
safe, and also protects future predictor labels containing punctuation or other
regex metacharacters.

## Stage 28.8.6.2 regex parser repair

The second transformed-predictor parser repair keeps closing parentheses in normalized
prediction text. The earlier normalization stripped a trailing `)` from questions
that ended with a transformed predictor such as `log(carat)`, so patterns of the
form `0.5 on log(carat)` did not match even though the predictor literal had been
escaped correctly.

The normalization now removes ordinary sentence-ending punctuation while preserving
meaningful variable-name punctuation. This keeps transformed terms such as
`log(carat)` intact before regex matching.

## Stage 28.8.8 four-covariate adjustment support

Manual testing of Diamonds IV showed that the existing three-covariate limit blocked the intended adjusted log-log example. Diamonds IV requires four predictors in the fitted formula: the primary `log(carat)` term plus the adjustment variables `cut`, `color`, and `clarity`.

Stage 28.8.8 relaxes the package and app covariate limit from three to four. The change is deliberately narrow: it allows the intended Diamonds IV model to fit, updates user-facing validation messages, and updates the model-setup predictor limiting logic so four selected predictors are retained instead of silently dropping the fourth.

This does not redesign higher-dimensional fitted-means displays. Any separate UI that explicitly says it is implemented only for one to three factor predictors remains unchanged unless later testing shows it blocks the Diamonds IV workflow.
