# Releasing spdesign to CRAN

A step-by-step guide to preparing, submitting and finishing a release. It
follows the checklist that `usethis::use_release_issue()` creates as a GitHub
issue, item by item and in the same order, so you can tick the issue off as you
go. Steps that are specific to spdesign are marked *spdesign*.

Replace `0.0.8` with the version you are releasing. The `tools/` folder is
excluded from the package build in `.Rbuildignore`, but kept in git.

The checklist assumes that you work directly on `master`. Here, the work
happens on a release branch with a pull request, so the GitHub checks run on
every push. Only the two commits at the end of the release, made by `usethis`,
go straight to `master`.

## Before you start

- Run R from Homebrew (`/opt/homebrew/bin/R`), the same R that Positron uses.
  See *Troubleshooting* for why this matters.
- Make sure the `spelling` and `urlchecker` packages are installed, and that
  `tidy` from Homebrew (`brew install tidy-html5`) is on your `PATH`.
- Check that the latest *R-hub weekly* run on the Actions tab passed.
- Open the release checklist:

  ```r
  usethis::use_release_issue("0.0.8")
  ```

## Prepare for release

### 1. Pull the latest changes

Switch to `master` and pull the latest changes:

```sh
git switch master
git pull
```

*spdesign:* then create the release branch, which contains the R-hub workflow
that `rhub::rhub_check()` needs:

```r
usethis::pr_init("release-0.0.8")
```

### 2. Check the current CRAN check results

<https://cran.r-project.org/web/checks/check_results_spdesign.html>

Fix any problem reported there on the release branch.

### 3. Polish NEWS

Edit the bullets under `# spdesign (development version)` in `NEWS.md`. Leave
the heading as it is: `use_version()` in step 11 replaces it with the version
number.

Put code in backticks. Text such as `x1[1:3](2:6)` is otherwise read as a
Markdown link, and CRAN reports it as an invalid URL.

### 4. Set the GitHub links

```r
usethis::use_github_links()
```

Sets the `URL` and `BugReports` fields in `DESCRIPTION` to the GitHub
repository. Keep the package website in `URL`.

*spdesign:* push the branch and open the pull request, so that R-hub can check
the branch in step 7 and the GitHub checks run on every push from here on:

```r
usethis::pr_push()
```

This opens the page for creating the pull request in your browser. Click
*Create pull request*.

### 5 to 8. Run the checks

Steps 5 to 8, together with the *spdesign* checks, are run in order by:

```r
source("tools/cran-prep.R")
```

The commands it runs are listed below, so you can also run them one at a time.

#### 5. Check the URLs

```r
urlchecker::url_check()
```

URLs in the package. The badges in `README.md` are not part of the package,
because `README.md` is in `.Rbuildignore`.

#### 6. Render the README

```r
devtools::build_readme()
```

Renders `README.Rmd` to `README.md`. Commit `README.md` if it changed.

*spdesign:* check the spelling:

```r
devtools::spell_check()
```

Add correctly spelled words to `inst/WORDLIST` with:

```r
spelling::update_wordlist()
```

#### 7. Check the package locally

```r
devtools::check(remote = TRUE, manual = TRUE)
```

A local check with CRAN's remote checks and the PDF manual. The version is
still the development version, e.g. `0.0.7.9000`, so CRAN's incoming check
reports a NOTE that the version contains large components. This NOTE is
expected and disappears after step 11.

*spdesign:* check the branch on GitHub with R-hub, on the platforms in
`tools/rhub-platforms.txt`. The results appear on the Actions tab.

```r
rhub::rhub_check(platforms = readLines("tools/rhub-platforms.txt"))
```

#### 8. Check the package on win-builder

```r
devtools::check_win_devel()
```

CRAN's win-builder on R-devel. The results arrive by email to the maintainer.

*spdesign:* also check on R-release:

```r
devtools::check_win_release()
```

The mac builder (`devtools::check_mac_release()`) is not used, because macOS is
covered by the local check, R-hub and the pull request, and the service is
often unavailable.

A complete log ends with `* DONE` and a `Status:` line. A log that stops in the
middle of a step, e.g. *checking CRAN incoming feasibility*, was cut off on
the server. Submit it again later.

Fix anything that comes up on the branch, and push again. The pull request
checks run again on every push, but R-hub and win-builder do not. Run those
again if you change files that are part of the package.

### 9. Update the comments to CRAN

Update `cran-comments.md` with the test environments and the results of steps
7 and 8.

### 10. Push the release branch

```sh
git push
```

## Submit to CRAN

### 11. Set the version

```r
usethis::use_version("patch") # or "minor" or "major"
```

This sets `Version` in `DESCRIPTION`, replaces the
`# spdesign (development version)` heading in `NEWS.md` with
`# spdesign 0.0.8`, and commits both. Run it only once per release: running it
again increases the version again. Then push the branch:

```sh
git push
```

### 12. Submit the package

```r
devtools::submit_cran()
```

`submit_cran()` creates a `CRAN-SUBMISSION` file with the submitted commit,
which step 15 uses. **Do not commit this file.** `use_github_release()` deletes
it after the release without committing the deletion, which would otherwise
leave an uncommitted change on `master`. The file is listed in
`.Rbuildignore`, so it is not included in the package.

### 13. Approve the submission

Confirm the submission through the link that CRAN emails to the maintainer.

## Wait for CRAN...

If CRAN asks for changes, make them on the release branch, push, and
resubmit:

```r
devtools::submit_cran()
```

This updates `CRAN-SUBMISSION`. Add a short *Resubmission* section at the top
of `cran-comments.md` that lists what you changed.

### 14. Accepted :tada:

*spdesign:* merge the pull request on GitHub with *Create a merge commit*, not
*Squash and merge*. A merge commit keeps the submitted commit in the history of
`master`, so the release tag points to exactly the version CRAN received. Then
switch to `master`, pull the merge, and delete the release branch locally and
on GitHub:

```r
usethis::pr_finish()
```

### 15. Create the GitHub release

```r
usethis::use_github_release()
```

Pushes `master`, creates the `v0.0.8` tag on the submitted commit and the
GitHub release with the release notes from `NEWS.md`, and deletes
`CRAN-SUBMISSION`.

### 16. Start the development version

```r
usethis::use_dev_version(push = TRUE)
```

Sets the version to `0.0.8.9000`, adds a `# spdesign (development version)`
heading to `NEWS.md`, and commits and pushes both to `master`.

Close the release issue.

## Troubleshooting

**The deletion of `CRAN-SUBMISSION` is left uncommitted after step 15.** The
file was committed in step 12. Commit the deletion on its own, or include it
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
