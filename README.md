
*photoSpec is a WORK IN PROGRESS and parts of it are in pretty crude shape*

### photoSpec: A photographic spectrophotometer [v 0.1]

At this time, `photoSpec is a WORK IN PROGRESS`.  Do not trust anything in it.  The documentation is pretty thin and we won't perfect it just yet, too much is changing.  This is our first stab at the concept and we already know we will change the way several things work in a future generation [probably v 0.2].

`photoSpec` does check and build, at least until we break it again.

The functions which mostly work, _but may still change a lot_ are:

+ plotCIEchrom
+ selectCIExy (coming soon)
+ plotCIEselection
+ plotSampleCard
+ showCalColSpace
+ getGamutValues (not user facing)
+ getWhiteValues (not user facing)
+ plotCIEselection (not user facing)
+ computeSampleAbs (not user facing)

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "photoSpec", username = "bryanhanson", ref = "master")
library("photoSpec")
````
If you use `ref = "devel"` you can get the development branch if it is available (and there may be other branches out there too).  Development branches have new, possibly incompletely tested features.  They may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.  If you are interested in a particular feature in the devel branch, you can probably just grab the function of interest and source it into an existing package installation.

Questions?  hanson@depauw.edu