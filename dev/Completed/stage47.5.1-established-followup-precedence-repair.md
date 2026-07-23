# Stage 47.5.1: established follow-up precedence repair

Stage 47.5 introduced deterministic guidance for questions that require an
alternative analysis. Its initial textual pre-classifier ran before the mature
follow-up classifier and therefore intercepted two already supported question
families.

This repair preserves the established classifier payload for:

- comparable-observation questions, including value-for-money or "good deal"
  requests; and
- conditional-quantile questions.

Comparable-observation questions continue through the existing model-answer
route. Conditional-quantile questions retain their established payload and are
then mapped by the shared route contract to `alternative_analysis_needed`.

All other Stage 47.5 text-based safeguards retain their earlier precedence so
that threshold-probability, causal, and diagnostic questions are not silently
answered by a less appropriate existing category.
