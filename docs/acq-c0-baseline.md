# ACQ-C0 local baseline

Baseline captured on 2026-09-30 with the workspace `.Rlib`, without live service,
model, or corpus access. Test command, run from the workspace parent:

```sh
R_LIBS_USER=/Users/barthoekstra/Development/biogain-tools/.Rlib Rscript -e 'devtools::test("/Users/barthoekstra/Development/biogain-tools/planscanR.screen/.claude/worktrees/acq-c0", reporter="summary", stop_on_failure=TRUE)'
```

Check command, run from this package root. For the retained run, `td` was
`/private/tmp/acq-c0-screen.Of2Ibg` (created with `mktemp -d`):

```sh
checkdir=$(mktemp -d /private/tmp/acq-c0-screen.XXXXXX)
R_LIBS_USER=/Users/barthoekstra/Development/biogain-tools/.Rlib Rscript -e 'td <- commandArgs(TRUE)[1]; result <- devtools::check(document=FALSE, cran=FALSE, vignettes=FALSE, manual=FALSE, error_on="never", check_dir=td); print(result)' "$checkdir"
```

Formatter self-check command (run from this package root):

```sh
R_LIBS_USER=/Users/barthoekstra/Development/biogain-tools/.Rlib Rscript scripts/check-styling.R .styler.R scripts/check-styling.R
```

Results (2026-09-30): the independent test run reported PASS 135, FAIL 0,
WARN 7, SKIP 0 and exited 0. The warnings are existing `glm.fit` convergence
warnings: three in `test-select_learning_curve.R:65`, two at `:113`, and one
each in `test-select_model.R:101` and `:130`; they are not ACQ-C0 changes.
`devtools::check()` exited 0: 0 errors, 0 warnings, 0 notes. Its testthat and
examples passed; vignettes were skipped. Repository indexes were attempted and
unreachable during dependency checking; locally installed dependencies allowed
the check to complete. Raw R CMD check log and included test output:
`/private/tmp/acq-c0-screen.Of2Ibg/planscanR.screen.Rcheck/00check.log`.
`/private/tmp/acq-c0-screen.Of2Ibg/planscanR.screen.Rcheck/tests/testthat.Rout`.

The checker’s self-check command passed for `.styler.R` and
`scripts/check-styling.R`. It also passed a clean fixture (exit 0), rejected an
unformatted fixture (exit 1), left that fixture byte-identical (cmp exit 0),
and rejected no paths (exit 1).

Formatter checks take explicit R file paths and reject an empty list. Files are
copied to a temporary directory, formatted there, and byte-compared; the source
tree is never changed. `.styler.R` sets the tidyverse style with two-space
indentation. This baseline does not reformat existing source. The existing
`R/relevance_model.R` does not conform to the new style, which is outside this
docs-and-setup task and was not changed.
