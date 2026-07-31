# Stage 49.8.7: routing test fixture repair

Stage 49.8.7 repairs the focused routing tests introduced in Stage 49.8.6.

The original comparison test used four observations for a three-parameter linear model and produced an essentially perfect fit. Because the package test suite converts warnings to errors, calling `vcov()` on that model failed before the routing behaviour could be checked.

This repair:

- replaces the exact-fit fixture with a larger, non-degenerate data set;
- renames the test file and test descriptions so tests describe behaviour rather than development-stage numbers; and
- removes the obsolete stage-numbered test file during installation.

No package behaviour is changed by this repair.
