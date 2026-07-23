# Stage 47.9.1: specialised response replacement repair

Stage 47.9 correctly recognised the twelve focused intelligent-question examples,
but the ordinary fitted-model summary was still appended after each deterministic
route response.

This repair:

- returns `question_route_response` text directly instead of appending it to the
  language-model explanation;
- preserves the established append behaviour for predictions, observation
  questions and other deterministic model answers;
- adds coverage for all twelve focused questions;
- repairs two grammatical defects in deterministic guidance; and
- restores successful-run removal of the consumed change-set archive.
