# spdesign v0.0.3
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
