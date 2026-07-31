# Stage 49.8.14: LLM boundary diagnostics

Stage 49.8.14 adds diagnostic capture around explanation generation so the
question-routing evaluation can distinguish language-model output from later
WMFM processing.

For each explanation, the evaluation output now records:

- whether the explanation cache was used;
- whether the language model was called;
- the exact prompt sent to the provider;
- the raw provider response;
- the response after numeric-expression normalisation;
- the text after deterministic research-question and follow-up assembly;
- the text after general explanation post-processing; and
- the final explanation returned by `runModel()`.

The capture is stored in model metadata and exposed in evaluation JSON under
`diagnostics.explanationGeneration`. It does not alter the student-facing
explanation. Cached results are marked explicitly and do not pretend to have a
raw provider response for the current run.


## Stage 49.8.14.1 repair

The first Stage 49.8.14 runner exposed an uninitialised `generation` object in the diagnostics JSON helper. This repair initialises the generation diagnostics to an empty list when older or unrelated diagnostics payloads do not contain `explanationGenerationDiagnostics`, preserving compatibility with the existing Developer Mode diagnostics tests.
