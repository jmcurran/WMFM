# Stage 28.8.1 - Log-log guidance test fix

## Purpose

Stage 28.8 added a regression test to ensure student-facing log-log guidance uses proportional-change wording and avoids specialised terminology. Local testing showed that the guidance text itself still named the terms it was telling the LLM to avoid, so the test correctly failed.

## Change

The guidance now asks the LLM to avoid specialist jargon for this relationship without naming the excluded terms in the student-facing instruction block. This keeps the prompt payload aligned with the Stage 28 terminology decision:

- use log-log terminology internally
- use proportional-change wording externally
- describe original-scale behaviour as multiplicative rather than additive

## Version

This repair stage should set the package version to `0.2.4.010`.
