# Stage 43.5: full validation and installation

Stage 43.4 added the labelled manual prediction-evaluation prompts but deliberately used the no-validation workflow because only a developer-facing Markdown file changed.

That decision did not meet the purpose of the stage. The manual examples are intended to exercise the installed Stage 43 prediction behaviour, so the package used for evaluation must first pass the complete WMFM validation workflow and be installed from the exact source package built after validation.

Stage 43.5 therefore performs no further prediction-engine changes. It explicitly forces:

- the R string-escape preflight;
- roxygen2 documentation generation;
- the complete strict test suite;
- the standard strict package check;
- a git commit recording the validation stage;
- creation of `stage43.5_completed.zip`;
- source-package build; and
- installation of the exact built package.

After the runner exits successfully, the local `runStage` dispatcher may remove the downloaded outer bundle and extracted runner/change-set directory. The completed archive remains in the WMFM package root.
