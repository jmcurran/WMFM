# Stage 48.9.1: developer example test repair

Stage 48.9.1 repairs the developer-only Course explanation grading example test.

`loadExampleSpec()` returns the parsed example specification in `info$spec`, while loading the research question into the convenience field `info$researchQuestion`. The Stage 48.9 test incorrectly looked for the formula at `info$formula`. The repaired test checks `info$spec$formula`, matching the established return structure without changing application behaviour.
