# Stage 49.6.2: R string escape repair

Stage 49.6.2 repairs the paragraph-normalisation regular expressions introduced in Stage 49.6.1.

The whitespace expressions now use doubled backslashes in R string literals so that the package string-escape preflight accepts them while preserving the intended Perl-compatible regular-expression behaviour.
