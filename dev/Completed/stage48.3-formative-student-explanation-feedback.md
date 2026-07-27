# Stage 48.3: Formative student explanation feedback

Stage 48.3 adds the first feedback loop to the student-authored explanation workspace.

## User workflow

After fitting a model, the student can:

1. write an explanation;
2. use the statistical insertion toolbar when useful;
3. click **Check my explanation**;
4. review strengths and priorities for revision; and
5. revise and check the explanation again.

The check does not generate or reveal WMFM's model explanation.

## Grading approach

This first implementation uses the existing deterministic WMFM grading path. It
therefore remains fast, reproducible, and independent of the configured language
model provider. The feedback view deliberately exposes only a compact rubric score,
strengths, and revision priorities rather than the developer scoring record.

Editing the answer after a check clears the previous result and prompts the student
to check the revised explanation again. Fitting a new model clears both the answer
and its feedback.

## Deferred work

Later substages may add optional LLM-enriched feedback, revision history, model-aware
feedback wording, and comparison between successive attempts.
