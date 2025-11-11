##  Test environments
* local MacOS Sonoma 14.7, R 4.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs

There was 1 NOTE:
  Sklodowska and INSPiRE in description are names and not spelling mistakes.
  There is one call to saveRDS(). This is to allow users to save intermediate designs when generating them. This requires users to actively set the argument save_designs = TRUE in the generate_design() function. The default is FALSE to avoid modifying package users computers and environments without explicit consent.

## Downstream dependencies
There are currently no downstream dependencies for this package
