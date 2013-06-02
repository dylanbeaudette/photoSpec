
*photoSpec is a WORK IN PROGRESS*

### photoSpec: A photographic spectrophotometer [v 0.2]

At this time, `photoSpec is a WORK IN PROGRESS`.  Do not trust anything in it.  The documentation is quite possibly incorrect and out of sync and we won't perfect it just yet, too much is changing.

`photoSpec` does check and build and run, at least until we break it again.

#### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "photoSpec", username = "bryanhanson", ref = "master")
library("photoSpec")
````
If you use `ref = "devel"` you can get the development branch if it is available (and there may be other branches out there too).  Development branches have new, possibly incompletely tested features.  They may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.  If you are interested in a particular feature in the devel branch, you can probably just grab the function of interest and source it into an existing package installation.

Questions?  hanson@depauw.edu