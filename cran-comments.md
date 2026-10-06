## Test environments
* local macOS 27.0.1, R 4.6.1
* win-builder: R-devel (2026-10-05 r90641) and R-release (4.6.1)
* R-hub: linux, windows and macos-arm64 (R-devel), ubuntu-next (R 4.6.1 patched), atlas, mkl, nold, nosuggests and vnu
* GitHub Actions: macOS (release), Windows (release) and Ubuntu (devel, release and oldrel-1)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Additional comments
There is one call to saveRDS(). It allows users to save intermediate designs while they are generated. Users must actively set the argument save_designs = TRUE in generate_design(). The default is FALSE, so that nothing is written to the user's computer without explicit consent.

## Downstream dependencies
There are currently no downstream dependencies for this package.
