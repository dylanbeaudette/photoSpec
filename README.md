
### photoSpec: A photographic spectrophotometer

At this time, `photoSpec is a WORK IN PROGRESS`.  It has matured quite a bit but is still undergoing tests which may significantly change the code without warning.

photoSpec allows one to apply Beer's Law to a photograph containing color swatches (paint chips) and a sample to be analyzed.  The motivation is to provide a method for 'spectrophotometry' of objects that cannot be chemically analyzed because of their value or non-extractability.  Functions in the package cover the entire workflow from generating calibration colors to obtaining a quantitative answer analogous to concentration in Beer's Law.

#### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "photoSpec", username = "bryanhanson", ref = "master")
library("photoSpec")
````
If you use `ref = "devel"` you can get the development branch if it is available.  Development branches have new, possibly incompletely tested features.  They may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.

Questions?  hanson@depauw.edu