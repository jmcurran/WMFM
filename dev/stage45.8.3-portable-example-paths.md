# Stage 45.8.3: Portable observation-example paths

Stage 45.8.3 shortens the permanent directory and specification filenames used by the observation-question development examples.

The previous descriptive names were stage-independent but still exceeded portable tar-path limits because the long directory name was repeated in the specification filename.

The examples now use short descriptive directory identifiers and a common `example.spec.yml` filename:

- `resid-low`
- `resid-high`
- `resid-unusual`
- `diamond-neighbours`
- `diamond-percentile`
- `poisson-residual`

Display names and example behaviour are unchanged. The runner removes the obsolete long-path directories before installing the replacements.
