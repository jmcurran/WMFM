# Explaining the explanation - stage 3 claim-to-evidence mapping

## Context

I am working on an R package/app called WMFM. The latest code base is attached.

In the previous stages of this workflow, we have already built:

- a deterministic `explanationAudit` object that records structured inputs used to construct the model explanation
- a student-facing teaching summary layer derived from that audit
- GUI work that shows a student-facing "How this explanation was constructed" panel
- some optional tutor-style explanation support

Please inspect the existing code first and build on top of what is already there.

Do not redesign or replace the stage 1 and stage 2 work unless there is a very strong reason. The goal now is to extend it.

## What I want to assess and likely implement

The next natural stage appears to be a more refined **claim-to-evidence mapping** layer.

By this I mean something like:

- "this sentence in the explanation came from these model quantities"
- "this part of the explanation was supported by this coefficient, fitted value, anchor choice, or confidence interval"
- "this wording about uncertainty came from this interval-based evidence"

I want you to inspect the current code first and confirm whether this stage is still missing or only partially implemented.

My current understanding is:

- stage 2 gives a good student-facing teaching summary
- but it may not yet map specific explanation claims or sentences back to specific evidence items

If you agree, then implement stage 3.

## Goal for stage 3

Create a deterministic, inspectable mapping from the final explanation text to the specific evidence used to support it.

This should be useful pedagogically for students and also useful for QA.

## Important design principles

- Do not pretend to expose hidden chain-of-thought.
- Do not claim that the system is showing the model's private reasoning.
- Do not use theatrical wording like "the model thought..."
- Prefer deterministic mapping where possible.
- If the mapping is partly heuristic, say so explicitly in the code and in the UI wording.
- Keep the student-facing view educational and readable.

## What I think stage 3 should include

### 1. A claim-to-evidence mapping object

Build a new deterministic object, perhaps something like:

```r
buildExplanationClaimEvidenceMap(
  explanationText,
  audit,
  teachingSummary,
  model
)
```

This object should identify meaningful units from the final explanation and connect them to the evidence used.

Depending on what is most robust, the units might be:

- sentences
- clauses
- labelled claims

I do not mind which is used, as long as the result is understandable and reasonably stable.

### 2. Evidence types to map back to

Where relevant, map each unit of explanation back to things like:

- coefficient rows
- transformed effect quantities
n- anchored baseline fitted values
- numeric anchor choices and why they were chosen
- factor reference levels
- confidence interval rows
- research-question linkage
- scale or back-transformation choices

### 3. Student-facing GUI rendering

If appropriate, add a section to the existing explanation panel that lets a student see:

- a sentence or claim from the explanation
- a short note explaining what evidence supports it

This should be plain language and should not feel like a debugging dump.

It may be better as an accordion or expandable section.

### 4. QA-friendly structure

The underlying object should be deterministic and inspectable enough for testing.

I may also want to inspect it through the console or API for QA.

## Important implementation question

One thing I want you to think carefully about is this:

Should the mapping be based on the final generated explanation text itself, or should it be based on a smaller set of structured explanation claims assembled before or alongside the text?

Please choose the most robust design after inspecting the code.

My suspicion is that a hybrid may be best:

- deterministic evidence inventory underneath
- lightweight matching or labelling against the final explanation text

But I want your judgement after inspecting the current implementation.

## Existing files likely to matter

Please inspect at least these files if they still exist or their current equivalents:

- `R/model-explanationAudit.R`
- `R/model-explanationTeachingSummary.R`
- `R/app-explanationAudit-uiHelpers.R`
- `R/app-server.R`
- `R/model-lm-explanation.R`
- `R/prompt-explain.R`
- any helper files that build explanation prompts or deterministic explanation metadata

## Requirements

- Preserve the existing stage 1 and stage 2 behavior where sensible.
- Build on the current audit and teaching summary infrastructure.
- Keep the student-facing view plain language and educational.
- Add tests.
- Use real data in tests where possible to avoid fragile perfect-fit toy examples.
- Keep tests offline and deterministic.

## Coding preferences

- use `=` for assignment
- use camelCase
- always use braces
- use roxygen2 documentation
- prefer `@importFrom` over `pkg::fun()` in code
- preserve backward compatibility where sensible
- do not rename files with suffixes like `_fixed` or `_updated`

## What I want from you

Please:

1. inspect the current code and confirm whether stage 3 is still needed
2. propose the best design for a first implementation of claim-to-evidence mapping
3. implement it if appropriate
4. regenerate any modified files in full
5. add tests
6. provide a zip file of changed files
7. provide a plain-text ASCII git commit message

## Key reminder

This feature is for students learning how model interpretation works.

The final output should feel like:

- a careful explanation of where the explanation came from
- not a developer audit dump
- not fake chain-of-thought
