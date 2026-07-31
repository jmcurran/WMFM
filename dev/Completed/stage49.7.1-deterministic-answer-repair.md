# Stage 49.7.1: deterministic-answer precedence repair

Stage 49.7 connected `runModel()` and the developer evaluation suite to the
full research-question objective path. Its first full test run exposed a
precedence error in the final explanation assembly.

A question objective can retain route-level missing-information metadata even
when a later specialised builder has successfully produced a deterministic
prediction or comparison payload. Stage 49.7 checked `requiresFollowup` before
checking those payloads, so valid Stage 49.4 and Stage 49.5 answers were
replaced by a generic clarification.

This repair:

- builds any valid deterministic specialised answer first;
- uses deterministic clarification only when no specialised answer is
  available and follow-up is genuinely required;
- updates the older incomplete-profile expectation to the intended Stage 49.7
  clarification behaviour; and
- replaces a fragile test fixture with a small self-contained data frame that
  includes the required attendance variable.

The repair is deliberately narrow and does not change question classification,
prediction calculations, or evaluation-example metadata.
