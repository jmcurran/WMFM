# Stage 49.8.8: version-bump runner repair

Stage 49.8.7 installed its files successfully but failed while bumping the package version because the inline R expression used an escaped regular-expression separator that was not portable through the runner invocation.

This repair replaces the version split with `strsplit(..., fixed = TRUE)`, avoiding regular-expression escaping entirely. No package behaviour or tests are changed.
