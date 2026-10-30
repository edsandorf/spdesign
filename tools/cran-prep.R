# Checks to run before submitting spdesign to CRAN
#
# Run from the package root with source("tools/cran-prep.R"). The script runs
# steps 5 to 8 of the release checklist created by usethis::use_release_issue(),
# in the same order, together with the checks that are specific to spdesign.
# See tools/release.md for the full release.
#
# The checks on R-hub and win-builder send the package to external services
# and only run after you confirm:
#
# - R-hub checks the current branch on GitHub Actions, so push it first. The
#   results appear on the Actions tab of the repository.
# - CRAN's win-builder emails the results to the maintainer.

stopifnot(file.exists("DESCRIPTION"))

confirm <- function(question) {
  interactive() && utils::menu(c("Yes", "No"), title = question) == 1
}

# 5. URLs in the package
urlchecker::url_check()

# 6. Render README.Rmd to README.md
devtools::build_readme()

# spdesign: spelling. Add correctly spelled words to inst/WORDLIST with
# spelling::update_wordlist()
if (requireNamespace("spelling", quietly = TRUE)) {
  print(devtools::spell_check())
} else {
  message("Install the 'spelling' package to check the spelling.")
}

# 7. Local check with CRAN's remote checks and the PDF manual
devtools::check(remote = TRUE, manual = TRUE)

# spdesign: CRAN-like checks on R-hub. The platforms are also used by the
# weekly R-hub workflow in .github/workflows/rhub-weekly.yaml
platforms <- readLines("tools/rhub-platforms.txt")

if (
  confirm("Start the R-hub checks for the current branch on GitHub Actions?")
) {
  rhub::rhub_check(platforms = platforms)
}

# 8. CRAN's win-builder on R-devel, and on R-release for spdesign. The mac
# builder is not used: macOS is covered by the local check, R-hub and the pull
# request.
if (confirm("Submit to win-builder (devel and release)?")) {
  devtools::check_win_devel()
  devtools::check_win_release()
}
