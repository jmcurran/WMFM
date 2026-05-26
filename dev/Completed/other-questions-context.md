# WMFM work stream context: "Any other questions?" user instruction box for extra LLM requests

I am working on an R package/app called WMFM. The code base is attached.

## Goal
Add a UI affordance on the model/explanation side that lets the user ask the LLM to do **additional model-related tasks** beyond the standard explanation.

Examples:
- ask for a prediction at a particular data point, ideally with a confidence interval if supported
- ask for interpretation in terms of a 10-unit change in `x` instead of a 1-unit change
- ask for a more concise explanation
- ask for an explanation aimed at a different audience level

I referred to this as **"Any other questions?"** in the UI, but I am open to a better label.

## Important caution
I do **not** want this to become an unrestricted prompt injection box that can derail the app.
The extra request should stay constrained to legitimate model-explanation tasks.

## What I want
1. Add a button or expandable UI element that reveals a text box for extra requests.
2. Store that text in Shiny state.
3. Pass it into the explanation/prompt workflow in a controlled way.
4. Constrain the LLM so that the extra request is treated as a bounded extension of the app’s explanation task, not a free-form assistant chat.
5. Preserve the standard explanation when the box is empty.

## Current architecture I have observed
Relevant existing pieces include:
- `R/app-ui.R` for the Model and Fitted Model tab UI
- `R/app-server.R` for reactive state, model fitting, and explanation generation
- `R/prompt-explain.R` for the explanation prompt
- `R/prompt-core.R` for the shared language contract
- `R/model-lm-explanation.R` for the LLM call and explanation caching
- confidence-interval helper flows in:
  - `R/model-ci-data.R`
  - `R/model-ci-support.R`
  - `R/app-modelOutput-serverHelpers.R`

The app already has a structured explanation pipeline, so I would like this feature to plug into that pipeline rather than create a separate ad hoc chat mode.

## Design problem to solve
I want help deciding the safest and cleanest design.
Possible options include:
- **Option A:** a bounded free-text box, with prompt instructions that the request must stay within supported model tasks
- **Option B:** a semi-structured UI with a small set of allowed request types plus an optional free-text refinement
- **Option C:** a post-explanation "follow-up task" box that operates on the already-fitted model

I would like your recommendation, but I lean toward a constrained design rather than a completely open text box.

## Constraints I care about
- The feature should not break existing explanation behaviour.
- It should not let the model ignore the core WMFM explanation rules.
- It should not produce unsupported computations by bluffing.
- If the user asks for something outside what the app can support, the response should say so clearly.
- If some requests require deterministic computation rather than just LLM narration, I want that made explicit.

## Examples of requests I want to think about
These are the kinds of things I imagine users asking:
- "Give the interpretation for a 10-unit increase in test score instead of a 1-unit increase."
- "What is the predicted probability of passing for a student with test = 14 and attendance = Yes?"
- "Can you explain this more briefly for a beginner?"
- "Focus on the comparison between Washington and Southern California."

These examples are intentionally mixed. Some are:
- purely framing/style changes
- alternate interpretations of already-estimated quantities
- actual model-derived predictions or intervals

I want the design to distinguish these cases where necessary.

## Likely implementation direction
Please inspect the current code and recommend whether this should be:
- just prompt-layer augmentation
- partly prompt-layer and partly deterministic computation
- or a separate model-follow-up API/UI path

I suspect at minimum we may need:
- a new field in app state
- prompt instructions that explicitly bound the extra request
- validation/screening of the request
- possibly a whitelist/category system
- cache-key updates if explanation text depends on this new input

## Files that likely matter
Please inspect at least these files first:
- `R/app-ui.R`
- `R/app-server.R`
- `R/prompt-explain.R`
- `R/prompt-core.R`
- `R/model-lm-explanation.R`
- `R/model-ci-data.R`
- `R/model-ci-support.R`
- `R/app-modelOutput-serverHelpers.R`
- possibly `R/api-runModel.R`

## My coding preferences
- use `=` for assignment
- use camelCase
- always use braces
- use roxygen2 documentation
- prefer `@importFrom` over `pkg::fun()` in code
- preserve backward compatibility where sensible
- keep helpers separate where sensible, but avoid pointless fragmentation

## What I want from you
Please:
1. inspect the current implementation
2. recommend a concrete design, including guardrails
3. explain which requests should be supported now versus deferred
4. implement the first version
5. regenerate any touched files in full
6. add tests where appropriate
7. provide downloadable files or a zip if multiple files are changed
8. give me a plain-text git commit message with no unicode

## Important behavioural requirement
The extra request box should extend the educational usefulness of WMFM without turning it into an unconstrained general chat interface.
