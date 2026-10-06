# Releasing spdesign to CRAN

A step-by-step guide to preparing, submitting and finishing a release.
Replace `0.0.8` with the version you are releasing. The `tools/` folder is
excluded from the package build in `.Rbuildignore`, but kept in git.

The work happens on a release branch with a pull request, so the checks run on
every push. Only the two commits in step 8, made by `usethis`, go straight to
`master`.

## Before you start

- Run R from Homebrew (`/opt/homebrew/bin/R`), the same R that Positron uses.
  See *Troubleshooting* for why this matters.
- Make sure the `spelling` and `urlchecker` packages are installed, and that
  `tidy` from Homebrew (`brew install tidy-html5`) is on your `PATH`.
- Switch to `master` and run `git pull`.
- Check that the latest *R-hub weekly* run on the Actions tab passed.
- Check the current CRAN check results for the package:
  <https://cran.r-project.org/web/checks/check_results_spdesign.html>

## 1. Open the release checklist

```r
usethis::use_release_issue("0.0.8")
```

This creates a GitHub issue with `usethis`'s release checklist, which you
tick off as you go. That checklist assumes you work directly on `master`. Follow
this guide for the order of the steps, and use the issue to keep track.

## 2. Create the release branch

```r
usethis::pr_init("release-0.0.8")
```

Run this on `master`. It pulls the latest changes and creates the branch, which
then contains the R-hub workflow that `rhub::rhub_check()` needs.

## 3. Prepare the release

```r
usethis::use_version("patch") # or "minor" or "major"
```

- `use_version()` sets `Version` in `DESCRIPTION`, replaces the
  `# spdesign (development version)` heading in `NEWS.md` with
  `# spdesign 0.0.8`, and offers to commit both. Earlier headings use the
  `v0.0.7` style. Both work with `use_github_release()`.
- Put code in `NEWS.md` in backticks. Text such as `x1[1:3](2:6)` is
  otherwise read as a Markdown link, and CRAN reports it as an invalid URL.
- Update `cran-comments.md`: the test environments and the check results
  from step 5.

## 4. Push the branch and open the pull request

```r
usethis::pr_push()
```

This pushes the branch and opens the page for creating the pull request in
your browser. Click *Create pull request*. R-CMD-check, test coverage and the
pkgdown build then run automatically.

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
   Results arrive by email to the maintainer. The mac builder can be skipped,
   because macOS is covered by the local check, R-hub and the pull request.

Fix anything that comes up on the branch, and push again. The pull request
checks run again on every push, but R-hub, win-builder and the mac builder do
not. Run those again if you change files that are part of the package.

Record the results in `cran-comments.md`.

## 6. Submit to CRAN

Commit and push all changes first. The submitted commit must be on GitHub for
step 8.

```r
devtools::submit_cran()
```

Confirm the submission through the link that CRAN emails to the maintainer.

`submit_cran()` creates a `CRAN-SUBMISSION` file with the submitted commit,
which step 8 uses. **Do not commit this file.** `use_github_release()` deletes
it after the release without committing the deletion, which would otherwise
leave an uncommitted change on `master`. The file is listed in
`.Rbuildignore`, so it is not included in the package.

## 7. While CRAN reviews the package

If CRAN asks for changes, make them on the release branch, push, and
resubmit with `devtools::submit_cran()`, which updates `CRAN-SUBMISSION`. Add
a short *Resubmission* section at the top of `cran-comments.md` that lists
what you changed.

## 8. After CRAN accepts the package

Merge the pull request on GitHub with *Create a merge commit*, not *Squash and
merge*. A merge commit keeps the submitted commit in the history of `master`,
so the release tag points to exactly the version CRAN received.

```r
usethis::pr_finish()
usethis::use_github_release()
usethis::use_dev_version(push = TRUE)
```

- `pr_finish()` switches to `master`, pulls the merge, and deletes the
  release branch locally and on GitHub.
- `use_github_release()` pushes `master`, creates the `v0.0.8` tag on the
  submitted commit and the GitHub release with the release notes from
  `NEWS.md`, and deletes `CRAN-SUBMISSION`.
- `use_dev_version(push = TRUE)` sets the version to `0.0.8.9000`, adds a
  `# spdesign (development version)` heading to `NEWS.md`, and commits and
  pushes both to `master`.
- Close the release issue.

## Troubleshooting

**The deletion of `CRAN-SUBMISSION` is left uncommitted after step 8.** The
file was committed in step 6. Commit the deletion on its own, or include it
with the next change on a branch:

```sh
git rm CRAN-SUBMISSION
git commit -m "Remove CRAN-SUBMISSION after the release"
```

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

**R-devel reports calls to `structure()` using deprecated special names.**
Use `dim`, `dimnames`, `names` and `levels` instead of `.Dim`, `.Dimnames`,
`.Names` and `.Label`, e.g. `structure(1:4, dim = c(2L, 2L))`.
