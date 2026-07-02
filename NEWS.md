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
