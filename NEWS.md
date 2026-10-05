# spdesign v0.0.7
* New function build_candidate_set() builds the candidate set one alternative at a time instead of from the full factorial of all alternatives. Exclusions that refer to a single alternative are applied to its profiles before the alternatives are combined, and choice tasks that only differ in the order of exchangeable alternatives, e.g. two unlabelled alternatives, are only included once. This uses far less memory for large designs. generate_design() now uses it when no candidate set is supplied. Designs with exchangeable alternatives generated with a given seed will differ from earlier versions.
* New control option allow_reversed_pairs in generate_design() to include the profiles of exchangeable alternatives in every order. A warning is given if this results in a candidate set with more than a million rows.
* Fixed a bug where an exclusion on an attribute whose name starts with the name of another attribute, e.g. alt1_x10 and alt1_x1, was applied wrongly and could exclude every row of the candidate set.
* full_factorial() is deprecated. Use build_candidate_set() to build a candidate set from the utility functions, or expand.grid() for a list of attributes.
* Fixed a bug where level occurrences were applied to alternatives without the attribute when its name appeared inside another name, e.g. x1 inside x10 or b inside b_sq. The level occurrences could then never be satisfied and the search ran forever.
* Fixed a bug where a single range of level occurrences, e.g. `x1[1:3](2:6)`, could be applied to the number of levels of another attribute whose name starts with the same name, e.g. x10.
* Fixed a bug where a supplied candidate set was checked for attributes that an alternative does not have when its name appeared inside another name, e.g. x1 inside x10.
* Fixed a bug where the C-error used more than one parameter in the denominator when another parameter name starts with the name given in dudx, e.g. b_x1 and b_x12.
* The search for a design candidate that satisfies the level occurrences now stops with an informative error after 100,000 attempts instead of running forever.
* Utility functions where a prior is not followed by '*' and its attribute, e.g. 'x1 * b_x1' or 'b_x1 / x1', now give an informative error.
* BREAKING CHANGE: Dummy-coded attributes must now have the levels 1, 2, ..., K, where 1 is the base level, and exactly K - 1 priors. generate_design() returns an error otherwise. Previously, a prior without a matching level gave a misleading error about a singular Fisher information matrix. Attribute names ending in '_dummy', e.g. x1_dummy, are also an error, because the '_dummy' extension belongs on the parameter, e.g. b_x1_dummy. Previously, this gave an error about parameters named 'NA2' and 'NA3' without a prior. Other levels, e.g. negative, decimal or unsorted levels, could give wrongly named or wrongly ordered dummy-coded attributes. Only the number of levels matters for the design, so recode meaningful levels, e.g. c(1500, 750, 500), as c(1, 2, 3).
* Fixed a bug where interaction terms, e.g. I(x1 * x2), were left out of the Fisher information matrix. This also caused the warning 'longer object length is not a multiple of shorter object length'. The variance-covariance matrix can no longer be silently recycled to the wrong size. If a term or parameter in the utility functions cannot be matched to the design, an informative error is returned.
* Fixed a bug where a generic parameter was treated as alternative specific when its attribute has a different name in each alternative, e.g. b_time * time_car and b_time * time_bus.
* Fixed a bug where the variance-covariance matrix of designs with dummy-coded attributes was labelled with the wrong priors (see also the breaking change). The S-error and C-error were calculated with the wrong priors. The D-error and A-error, and therefore designs optimized for them, were not affected.
* Fixed the parsing of attribute names. Squared terms, e.g. I(x1^2), interactions written without spaces, e.g. I(x1*x2), and attribute names starting with a capital I now work.
* Evaluating a design candidate is faster because the utility functions are parsed fewer times.
* Random design candidates that must satisfy level occurrence restrictions are now found by swapping single rows until the restrictions are met, instead of redrawing the whole design. This is much faster when restrictions are tight. Used for the 'random' algorithm and the starting design of the 'federov' algorithm.
* Fixed a bug in the level occurrence check where a level missing from the design candidate caused counts to be compared against the wrong levels. Designs could be wrongly accepted or rejected. Affected speed of convergence to better designs. 
* Rewrote the search loop of the 'federov' algorithm. Swaps that do not improve the design are now discarded instead of kept, so the search always builds on the best design found. Affected speed of convergence to better designs. The initial design is evaluated before swapping starts. When a full pass finds no improving swap, the search restarts from a new random design candidate. The best design across all runs is returned as the design, and the best design of each run is stored in the new list element 'runs'. Swaps that would duplicate a row or violate the level occurrences are skipped without being evaluated. Designs generated with a given seed will differ from earlier versions.
* The 'federov' algorithm now continues through the candidate set where it left off when moving to the next row of the design, instead of starting again from the first row of the candidate set. Every row of the candidate set is now tried equally often, so the search reaches deeper into large candidate sets.
* The rewrite of the 'federov' algorithm fixes several errors: the candidate set index could run past the end of the candidate set, causing a subscript out of bounds error or an endless search, when repairing level occurrences or when a design candidate had a singular Fisher matrix; repairing level occurrences could add duplicate rows to the design; and a singular first design caused a "missing value where TRUE/FALSE needed" error.
* Fixed errors in the 'random' algorithm when a design candidate had a singular Fisher matrix: a singular first design caused a "missing value where TRUE/FALSE needed" error, and consecutive singular designs skipped the stopping conditions so the search could run past the maximum number of iterations.
* Removed the unused import of dplyr::distinct().
* Errors are no longer silently swallowed by generate_design(), block() and the search algorithms. Previously, any error, including errors from argument checks in generate_design(), returned an incomplete design object instead of stopping. Interrupting the search or blocking with Esc or Ctrl + C still returns the best design or blocking found so far.
* generate_design() now stops with an informative error if no design with a non-singular Fisher information matrix is found.
* A supplied candidate set may still contain attribute levels that are not listed in the utility functions, but generate_design() now stops with an informative error if this happens for an attribute with level occurrences specified.
* Fixed two bugs in the 'rsc' algorithm when design candidates had a singular Fisher matrix: consecutive singular designs skipped the stopping conditions so the search could run forever, and the warning after 1000 singular design candidates was never shown.
* Minor bug fixes and improvements

# spdesign v0.0.6
* Updated package dependency to R 4.1.0 because it relies on the |> operator. 
* Fixed a bug that would cause parsing of the utility functions to fail if an attribute contained "b_" at some point in the string. "b_" was the target for a regex looking for priors. The regex is updated to only consider "b_" at the beginning of a word.
* Fixed a bug that would cause the generate_design() function to fail if all priors were specified as Bayesian. The bug was caused by a check returning a NULL object that would then be expanded to match the number of draws. 
* Added the option save_designs to the generate_design function. When set to TRUE, all intermediate designs generated during the search process will be saved as .rds files in the current working directory. The default value is FALSE. 
* If you have not specified attribute level occurrence restrictions, all level occurence checks will be skipped. This ensures that you can supply a candidate set without specifying all levels in the utility functions. However, if you do specify level occurrence restrictions, then you must ensure that all levels used in the candidate set is also listed in the utility functions, otherwise you will get a subscript out of bounds error. This may or may not be changed later depending on feedback. Should not be a breaking change. Syntax documentation is updated to reflect this change.
* Changed the default efficiency threshold to be arbitrarily small to avoid issues where the search process would stop at the first iteration.
* Code linting and formatting updates
* Minor bug fixes

# spdesign v0.0.5
* Removed a check for all levels existing in the supplied candidate set. This caused errors when using restrictions on attribute level occurrence and a supplied candidate set. 
* Added a check to the modified federov algorithm to ensure that the new candidate row from the candidate set does not already exist in the design candidate.
* Minor bug fixes

# spdesign v0.0.4
* Added function level_balance() that produces a list of level occurrences in the design to inspect level balance
* Updates to documentation, examples, and syntax description
* Minor bug fixes

# spdesign v0.0.3
* Fixed a bug related to optimizing for c-efficiency where it would sometimes fail to correctly identify the denominator. 
* Fixed several bugs related to using a supplied candidate set with alternative specific constants and attributes. Checks have been updated. The code will now also add zero-columns for alternative specific constants and attributes in the utility functions where they are not present. This ensures that all matrices used when calculating the first- and second-order derivatives of the utility functions are square. 
* Fixed an issue where it failed to catch a mismatch in naming between the supplied candidate set and the utilty functions which caused hard to debug situations. Error messages should now catch this and provide additional information to help find the cause. Syntax is updated to reflect this as well. 
* Fixed an issue where the full factorial would be generated even when the "rsc" algorithm was used, which caused memory issues for large designs. It is now only generated for the "random" and "federov" algorithms. A small section is added to the syntax vignette to clarify this. 
* Minor bug fixes

# spdesign v0.0.2
* New function ´probabilities()´ will now return the choice probabilities by choice task. 
* Suppress warnings when calculating the correlation between the blocking column and the attributes to avoid warning when calculating correlation with respect to a constant.
* After a number of candidates without improvement try a new design candidate when using the RSC algorithm
* Updated package load message
* Fixed roxygen @docType issue

# spdesign v0.0.1
* This is the first working version of the `spdesign` package that is able to create simple efficient designs for the MNL model. 
