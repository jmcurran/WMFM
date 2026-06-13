# WMFM future development ideas after CRAN hardening

## Context

This file records the future-development ideas separated from the CRAN-readiness audit that opened Stage 38. The CRAN-hardening stream should stay focused on CRAN policy compliance, examples, external-provider behavior, package metadata, and public-release documentation. The ideas below are useful follow-on work, but they should not be mixed into the CRAN submission preparation unless a specific item is needed for release safety.

## 1. Explanation quality polish

Continue improving generated explanations without adding major new statistical machinery.

Good targets:

- Confidence interval phrasing, especially avoiding `could plausibly lie`.
- Repetition removal.
- Better student-facing language for adjustment context, for example `varies across pictures` rather than `varies depending on the adjustment context`.
- More consistent concluding sentences that directly answer the research question.

## 2. Release-mode versus developer-mode boundary

Before CRAN, make sure the app has a clean release-mode experience and developer diagnostics remain hidden unless deliberately enabled.

Good targets:

- Developer-mode UI smoke tests.
- Provider settings smoke tests.
- A release-mode app startup test.
- A public-user workflow test that never exposes internal grading/scoring controls.

## 3. Example library polish

The examples are now numerous. A useful later stage would curate them for public and classroom use.

Good targets:

- Separate classroom examples from internal test examples.
- Add example metadata such as difficulty, model family, and teaching topic.
- Provide a smaller public example list by default, with internal/test examples hidden or developer-only.

## 4. Scoring and grading report UX

The deterministic and LLM scoring systems have become substantial. A later development stream could make them more useful for teaching and debugging.

Good targets:

- Cleaner grade summaries.
- Better explanation of why a score was assigned.
- A compact `what to improve` section.
- Comparison reports across repeated runs.

## 5. Follow-up question UX

The deterministic follow-up machinery is mature but could be made more discoverable.

Good targets:

- Suggested follow-up prompts.
- Clearer UI when a follow-up cannot be answered deterministically.
- A small library of supported follow-up question templates.
- Better explanation of why a requested prediction is extrapolative.

## 6. API key setup guidance for novice users

A future README or documentation stage should add plain-language instructions for obtaining and configuring API keys for OpenAI and Anthropic.

This is important because novice users may find provider account setup, billing activation, API-key creation, and environment-variable configuration difficult even when the package code itself is working correctly.

Good targets:

- Add a short README section explaining that external LLM providers require a separate account and API key.
- Link users to the relevant provider dashboards without assuming they already understand developer accounts.
- Explain, in beginner-friendly language, the difference between a ChatGPT or Claude subscription and an API key.
- Show how to set keys with environment variables, for example `OPENAI_API_KEY` and `ANTHROPIC_API_KEY`.
- Mention that users should not commit API keys to Git, scripts, examples, or public screenshots.
- Keep this guidance separate from file-organization work so it can be reviewed as public documentation.
