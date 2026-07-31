# Stage 49.6: Evaluation and prompt robustness

Stage 49.6 adds deterministic developer diagnostics for research-question alignment.

The diagnostics report the classified archetype, primary objective, supplied profile, missing information, essential-concept coverage, deterministic-answer placement, contradiction flags, explanation length, mode suitability, and an overall alignment score.

The first metric is deliberately strict: when WMFM has produced a deterministic answer, that answer must remain at the beginning of the final explanation. This makes LLM displacement visible rather than relying only on prompt compliance.

Focused offline tests cover an aligned linear-model prediction, displacement and invalid continuous-interval language for a logistic outcome, and concise, standard, and detailed length policies.
