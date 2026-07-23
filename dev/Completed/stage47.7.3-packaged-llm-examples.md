# Stage 47.7.3: packaged LLM review examples

Stage 47.7 was intended to add developer-only examples to the packaged WMFM
example library, not an assertion-based internal routing test suite.

This repair adds twelve developer-only examples under
`inst/extdata/examples`. They use existing packaged or package-supplied data
and cover unclear purpose, capability guidance, missing prediction inputs,
response-question mismatch, causal claims, model adequacy, comparable
observations, conditional percentiles, and Poisson exceedance questions.

The examples are marked `exampleAudience: developer`, so they remain hidden
from the ordinary classroom example list and are available in developer mode.
They are intended to be run through the normal WMFM LLM workflow and reviewed
qualitatively. No simulated data or route assertions are used.

The obsolete Stage 47.7 assertion script and its generated report are removed
by the stage runner before validation.
