# Stage 49.6.1: deterministic-first alignment repair

This repair makes the deterministic-first diagnostic robust when a GLM prediction payload does not carry a cached `deterministicResponse` string. The diagnostic now inspects the first paragraph for the fitted response-scale value and appropriate probability, expected-count, or prediction language. It therefore distinguishes a genuinely prediction-first answer from an explanation that places the deterministic result later.
