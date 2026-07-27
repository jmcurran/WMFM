# Stage 48.1.1: Model explanation UI test repair

Stage 48.1 correctly separated model fitting from optional LLM explanation generation, but the full test suite exposed one stale UI expectation.

The test still expected the earlier phrase:

```text
sentence support, reading guidance
```

That phrase is no longer part of the Model Explanation tab after the Stage 48.1 redesign. The tab now states that WMFM generates an explanation only when the user explicitly asks for one.

Stage 48.1.1 updates the UI test to assert the new deliberate-request wording. No application behaviour is changed by this repair.
