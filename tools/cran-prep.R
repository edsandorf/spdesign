# Checks to run before submitting spdesign to CRAN
#
# Run from the package root with source("tools/cran-prep.R"). Steps 1 to 3 run
# locally. Steps 4 and 5 send the package to external check services and only
# run after you confirm:
#
# - R-hub checks the current branch on GitHub Actions, so push it first. The
#   results appear on the Actions tab of the repository.
# - CRAN's win-builder and mac builder email the results to the maintainer.
#
# The full release checklist, including the steps after these checks, is
# created as a GitHub issue with usethis::use_release_issue().

stopifnot(file.exists("DESCRIPTION"))

confirm <- function(question) {
  interactive() && utils::menu(c("Yes", "No"), title = question) == 1
}

# 1. URLs in the package
urlchecker::url_check()

# 2. Spelling. Add correctly spelled words to inst/WORDLIST with
#    spelling::update_wordlist()
if (requireNamespace("spelling", quietly = TRUE)) {
  print(devtools::spell_check())
} else {
  message("Install the 'spelling' package to check the spelling.")
}

# 3. Local check with CRAN's remote checks and the PDF manual
devtools::check(remote = TRUE, manual = TRUE)

# 4. CRAN-like checks on R-hub. The platforms are also used by the weekly
#    R-hub workflow in .github/workflows/rhub-weekly.yaml
platforms <- readLines("tools/rhub-platforms.txt")

if (confirm("Start the R-hub checks for the current branch on GitHub Actions?")) {
  rhub::rhub_check(platforms = platforms)
}

# 5. CRAN's own build services
if (confirm("Submit to win-builder (devel and release) and the mac builder?")) {
  devtools::check_win_devel()
  devtools::check_win_release()
  devtools::check_mac_release()
}
