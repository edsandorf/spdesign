# Releasing spdesign to CRAN

A step-by-step guide to preparing, submitting and finishing a release.
Replace `0.0.7` with the version you are releasing. The `tools/` folder is
excluded from the package build in `.Rbuildignore`, but kept in git.

## Before you start

- Run R from Homebrew (`/opt/homebrew/bin/R`), the same R that Positron uses.
  See *Troubleshooting* for why this matters.
- Make sure the `spelling` and `urlchecker` packages are installed, and that
  `tidy` from Homebrew (`brew install tidy-html5`) is on your `PATH`.
- Check that the latest *R-hub weekly* run on the Actions tab passed.

## 1. Open the release checklist

```r
usethis::use_release_issue("0.0.7")
```

This creates a GitHub issue with a checklist that you tick off as you go.

## 2. Create the release branch

```r
usethis::pr_init("release-0.0.7")
```

The branch comes from `master`, so it contains the R-hub workflow that
`rhub::rhub_check()` needs.

## 3. Prepare the release

- Set `Version` in `DESCRIPTION`.
- Rename the top heading of `NEWS.md` from `# spdesign (development version)`
  to `# spdesign v0.0.7`.
- Put code in `NEWS.md` in backticks. Text such as `x1[1:3](2:6)` is
  otherwise read as a Markdown link, and CRAN reports it as an invalid URL.
- Update `cran-comments.md`: the test environments and the check results
  from step 5.

## 4. Push the branch and open the pull request

```r
usethis::pr_push()
```

This pushes the branch and opens the pull request in your browser. R-CMD-check,
test coverage and the pkgdown build run automatically on the pull request.

## 5. Run the pre-submission checks

```r
source("tools/cran-prep.R")
```

The script runs:

1. `urlchecker::url_check()`: URLs in the package.
2. `devtools::spell_check()`: spelling. Add correctly spelled words to
   `inst/WORDLIST` with `spelling::update_wordlist()`.
3. `devtools::check(remote = TRUE, manual = TRUE)`: a local check with CRAN's
   remote checks and the PDF manual.
4. R-hub on the platforms in `tools/rhub-platforms.txt`, after you confirm.
   R-hub checks the branch on GitHub, so push it first. Results appear on the
   Actions tab.
5. CRAN's win-builder (devel and release) and mac builder, after you confirm.
   Results arrive by email to the maintainer.

Fix anything that comes up on the branch, and push again. Record the
results in `cran-comments.md`.

## 6. Submit to CRAN

```r
devtools::submit_cran()
```

Confirm the submission through the link that CRAN emails to the maintainer.
`submit_cran()` creates a `CRAN-SUBMISSION` file, which step 8 uses. It is
listed in `.Rbuildignore`, so it is not included in the package.

## 7. While CRAN reviews the package

If CRAN asks for changes, make them on the release branch and resubmit with
`devtools::submit_cran()`. Add a short *Resubmission* section at the top of
`cran-comments.md` that lists what you changed.

## 8. After CRAN accepts the package

```r
usethis::pr_merge_main()     # or merge the pull request on GitHub
usethis::use_github_release()
usethis::use_dev_version()
```

- `use_github_release()` creates the `v0.0.7` tag and the GitHub release,
  with the release notes from `NEWS.md`.
- `use_dev_version()` sets the version to `0.0.7.9000` and adds a
  `# spdesign (development version)` heading to `NEWS.md`, so further work is
  marked as development. Commit and push the change.
- Close the release issue.

## Troubleshooting

**R crashes in the spelling step (segmentation fault in `hunspell`).** A
package with compiled code was installed by a different R than the one you are
running. Homebrew R and CRAN's `R.framework` of the same version share the
package library `~/Library/R/arm64/<version>/library`, and a package built for
one can crash the other. Reinstall the package from source with Homebrew R:

```sh
/opt/homebrew/bin/Rscript -e 'install.packages("hunspell", type = "source", repos = "https://cloud.r-project.org")'
```

To find other packages linked to the wrong R, check which `libR.dylib` they
use with `otool -L <library>/<package>/libs/<package>.so`.

**A NOTE about HTML validation problems with `<U+...>`.** R is running in a
locale without UTF-8, e.g. a shell where `LANG` is not set. Run the check in a
UTF-8 locale, e.g. `LANG=en_US.UTF-8`. Keeping `DESCRIPTION` in plain ASCII
avoids the problem altogether.

**R-hub `nosuggests` fails to rebuild the vignettes.** When CRAN checks the
package without its suggested packages, only the packages in `VignetteBuilder`
are available. The vignettes use `rmarkdown`, so `DESCRIPTION` must contain
`VignetteBuilder: knitr, rmarkdown`.
